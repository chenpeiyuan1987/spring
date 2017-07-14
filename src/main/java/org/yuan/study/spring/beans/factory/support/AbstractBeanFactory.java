package org.yuan.study.spring.beans.factory.support;

import java.beans.PropertyEditor;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.factory.BeanCreationException;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.BeanFactoryUtils;
import org.yuan.study.spring.beans.factory.BeanIsNotAFactoryException;
import org.yuan.study.spring.beans.factory.BeanNotOfRequiredTypeException;
import org.yuan.study.spring.beans.factory.FactoryBean;
import org.yuan.study.spring.beans.factory.FactoryBeanNotInitializedException;
import org.yuan.study.spring.beans.factory.NoSuchBeanDefinitionException;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.beans.factory.config.BeanPostProcessor;
import org.yuan.study.spring.beans.factory.config.ConfigurableBeanFactory;

public abstract class AbstractBeanFactory implements ConfigurableBeanFactory {
	/** Logger available to subclasses */
	protected final Log logger = LogFactory.getLog(getClass());
	
	/** Parent bean factory, for bean inheritance support */
	private  BeanFactory parentBeanFactory;
	
	/** Cache of singletons: bean name --> bean instance */
	private final Map<String,Object> singletonCache = new HashMap<String,Object>();
	
	/**  */
	private final Set<String> currentlyInCreation = Collections.synchronizedSet(new HashSet());
	
	public AbstractBeanFactory() {
	}

	public AbstractBeanFactory(BeanFactory parentBeanFactory) {
		this.parentBeanFactory = parentBeanFactory;
	}
	
	
	//-------------------------------------------------------------------
	// Implementation of BeanFactory interface
	//-------------------------------------------------------------------
	
	@Override
	public Object getBean(String name) throws BeansException {
		return getBean(name, null, null);
	}

	@Override
	public Object getBean(String name, Class<?> requiredType) throws BeansException {
		return getBean(name, requiredType, null);
	}
	
	public Object getBean(String name, Object[] args) throws BeansException {
		return getBean(name, null, args);
	}
	
	public Object getBean(String name, Class<?> requiredType, Object[] args) throws BeansException {
		String beanName = transformedBeanName(name);
		Object bean = null;
		
		synchronized (this.singletonCache) {
			bean = this.singletonCache.get(beanName);
		}
		if (bean != null) {
			bean = getObjectForSharedInstance(name, bean);
		}
		
		else {
			if (getParentBeanFactory() != null && !containsBeanDefinition(beanName)) {
				if (getParentBeanFactory() instanceof AbstractBeanFactory) {
					return ((AbstractBeanFactory) getParentBeanFactory()).getBean(name, requiredType, args);
				}
				else if (args == null) {
					return getParentBeanFactory().getBean(name, requiredType);
				}
				else {
					throw new NoSuchBeanDefinitionException(beanName, 
						"Cannot delegate to parent BeanFactory because it does not supported passed-in arguments");
				}
			}
			
			RootBeanDefinition mergedBeanDefinition = getMergedBeanDefinition(beanName, false);
			checkMergedBeanDefinition(mergedBeanDefinition, beanName, requiredType, args);
		
			if (mergedBeanDefinition.isSingleton()) {
				synchronized (this.singletonCache) {
					bean = this.singletonCache.get(beanName);
					if (bean == null) {
						if (logger.isDebugEnabled()) {
							logger.debug(String.format("Creating shared instance of singleton bean '%s'", beanName));
						}
						this.currentlyInCreation.add(beanName);
						try {
							bean = createBean(beanName, mergedBeanDefinition, args);
							addSingleton(beanName, bean);
						}
						catch (BeansException ex) {
							destroyDisposableBean(beanName);
							throw ex;
						}
						finally {
							this.currentlyInCreation.remove(beanName);
						}
					}
				}
				bean = getObjectForSharedInstance(name, bean);
			}
			else {
				bean = createBean(beanName, mergedBeanDefinition, args);
			}
		}
		

		if (requiredType != null && !requiredType.isAssignableFrom(bean.getClass())) {
			throw new BeanNotOfRequiredTypeException(name, requiredType, bean.getClass());
		}
		return bean;
	}

	@Override
	public Class<?> getType(String name) {
		return null;
	}

	@Override
	public boolean isSingleton(String name) {
		return false;
	}

	@Override
	public boolean containsBean(String name) {
		if (containsLocalBean(name)) {
			return true;
		}
		
		BeanFactory parentBeanFactory = getParentBeanFactory();
		if(parentBeanFactory != null) {
			return parentBeanFactory.containsBean(name);
		}
		return false;
	}

	@Override
	public String[] getAliases(String name) {
		// TODO Auto-generated method stub
		return null;
	}

	
	//-----------------------------------------------------------
	// Implementation of HierarchicalBeanFactory interface
	//-----------------------------------------------------------
	
	@Override
	public boolean containsLocalBean(String name) {
		return false;
	}

	@Override
	public BeanFactory getParentBeanFactory() {
		return this.parentBeanFactory;
	}

	
	//-----------------------------------------------------------
	// Implementation of ConfigurableBeanFactory interface
	//-----------------------------------------------------------
	
	@Override
	public void addBeanPostProcessor(BeanPostProcessor beanPostProcessor) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean containsSingleton(String beanName) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void destroySingletons(String beanName) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public int getBeanPostProcessorCount() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void registerAlias(String beanName, String alias) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void registerCustomEditor(Class<?> requiredType, PropertyEditor propertyEditor) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void registerSingleton(String beanName, Object singletonObject) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setParentBeanFactory(BeanFactory parentBeanFactory) {
		this.parentBeanFactory = parentBeanFactory;
	}
	
	
	//-----------------------------------------------------------
	// Implementation methods
	//-----------------------------------------------------------
	
	/**
	 * 
	 * @param name
	 * @return
	 */
	protected boolean isFactoryDereference(String name) {
		return BeanFactoryUtils.isFactoryDereference(name);
	}
	
	/**
	 * 
	 * @param name
	 * @return
	 */
	protected String transformedBeanName(String name) {
		return BeanFactoryUtils.transformedBeanName(name);
	}
	
	/**
	 * 
	 * @param name
	 * @param bean
	 * @return
	 */
	protected Object getObjectForSharedInstance(String name, Object bean) {
		String beanName = transformedBeanName(name);
		
		if (isFactoryDereference(name) && !(bean instanceof FactoryBean)) {
			throw new BeanIsNotAFactoryException(beanName, bean.getClass());
		}
		
		if (bean instanceof FactoryBean) {
			if (!isFactoryDereference(name)) {
				FactoryBean factory = (FactoryBean) bean;
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Bean with name '%s' is a factory bean", beanName));
				}
				try {
					bean = factory.getObject();
				}
				catch (Exception ex) {
					throw new BeanCreationException(beanName, "FactoryBean threw exception on object creation", ex);
				}
				if (bean == null) {
					throw new FactoryBeanNotInitializedException(beanName, "FactoryBean returned null object: " + 
						"probably not fully initialized (maybe due to circular bean reference)");
				}
			}
			else {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Calling code asked for FactoryBean instance for name '%s'", beanName));
				}
			}
		}
		
		return bean;
	}
	
	//-----------------------------------------------------------
	// Abstract methods to be implemented by subclasses
	//-----------------------------------------------------------
	
	/**
	 * 
	 * @param beanName
	 * @return
	 */
	protected abstract boolean containsBeanDefinition(String beanName);

	/**
	 * 
	 * @param beanName
	 * @return
	 */
	protected abstract BeanDefinition getBeanDefinition(String beanName) throws BeansException;
	
	/**
	 * 
	 * @param beanName
	 * @param mergedBeanDefinition
	 * @param args
	 * @return
	 * @throws BeanCreationException
	 */
	protected abstract Object createBean(String beanName, RootBeanDefinition mergedBeanDefinition, Object[] args) throws BeanCreationException;

	/**
	 * 
	 * @param beanName
	 * @param mergedBeanDefinition
	 * @return
	 */
	protected Class<?> getTypeForFactoryMethod(String beanName, RootBeanDefinition mergedBeanDefinition) {
		return null;
	}
}
