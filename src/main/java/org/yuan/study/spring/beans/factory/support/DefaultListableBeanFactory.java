package org.yuan.study.spring.beans.factory.support;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.factory.BeanCreationException;
import org.yuan.study.spring.beans.factory.BeanCurrentlyInCreationException;
import org.yuan.study.spring.beans.factory.BeanDefinitionStoreException;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.BeanFactoryUtils;
import org.yuan.study.spring.beans.factory.FactoryBean;
import org.yuan.study.spring.beans.factory.NoSuchBeanDefinitionException;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.beans.factory.config.ConfigurableListableBeanFactory;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.StringUtils;

public class DefaultListableBeanFactory 
	extends AbstractAutowireCapableBeanFactory implements ConfigurableListableBeanFactory, BeanDefinitionRegistry {

	/** Whether to allow re-registration of a different definition with the same name */
	private boolean allowBeanDefinitionOverriding;
	
	/** Map of bean definition objects, keyed by bean name */
	private final Map<String,BeanDefinition> beanDefinitionMap = new HashMap<String,BeanDefinition>();
	
	/** List of bean definition names, in registration order */
	private final List<String> beanDefinitionNames = new ArrayList<String>();
	
	
	/**
	 * Create a new DefaultListableBeanFactory
	 */
	public DefaultListableBeanFactory() {
		super();
	}
	
	
	/**
	 * Create a new DefaultListableBeanFactory with the given parent.
	 * @param parentBeanFactory
	 */
	public DefaultListableBeanFactory(BeanFactory parentBeanFactory) {
		super(parentBeanFactory);
	}


	//-----------------------------------------------------------------
	// Implementation of methods
	//-----------------------------------------------------------------
	
	/**
	 * Return whether it should be allowed to override bean definition by registering
	 * a different definition with the same name, automatically replacing teh former.
	 * @return
	 */
	public boolean isAllowBeanDefinitionOverriding() {
		return allowBeanDefinitionOverriding;
	}

	/**
	 * Set whether it should be allowed to override bean definitions by registering 
	 * a different definition with the same name, automatically replacing the former.
	 * If not, an exception will be thrown. Default is "true".
	 * @param allowBeanDefinitionOverriding
	 */
	public void setAllowBeanDefinitionOverriding(boolean allowBeanDefinitionOverriding) {
		this.allowBeanDefinitionOverriding = allowBeanDefinitionOverriding;
	}
	
	/**
	 * Determine whether the bean definition with the given name matches the given type.
	 * @param beanName
	 * @param type
	 * @return
	 */
	private boolean isBeanDefinitionTypeMatch(String beanName, Class<?> type) {
		if (type == null) {
			return true;
		}
		RootBeanDefinition rootBeanDefinition = getMergedBeanDefinition(beanName, false);
		return (rootBeanDefinition.hasBeanClass() && type.isAssignableFrom(rootBeanDefinition.getBeanClass()));
	}
	
	/**
	 * Check whether the specified bean matches the given type.
	 * @param beanName
	 * @param type
	 * @return
	 */
	private boolean isBeanTypeMatch(String beanName, Class<?> type) {
		if (type == null) {
			return true;
		}
		Class<?> beanType = getType(beanName);
		return (beanType != null && type.isAssignableFrom(beanType));
	}
	
	
	//-----------------------------------------------------------------
	// Implementation of ConfigurableListableBeanFactory class
	//-----------------------------------------------------------------
	
	@Override
	public void preInstantiateSingletons() throws BeansException {
		if (logger.isInfoEnabled()) {
			logger.info(String.format("Pre-instantiating singletons in factory [%s]", this));
		}
		
		try {
			for (String beanName : beanDefinitionNames) {
				if (!containsSingleton(beanName) && containsBeanDefinition(beanName)) {
					RootBeanDefinition beanDefinition = getMergedBeanDefinition(beanName, false);
					if (!beanDefinition.isAbstract() && beanDefinition.isSingleton() && !beanDefinition.isLazyInit()) {
						if (beanDefinition.hasBeanClass() && FactoryBean.class.isAssignableFrom(beanDefinition.getBeanClass())) {
							FactoryBean factory = (FactoryBean) getBean(FACTORY_BEAN_PREFIX + beanName);
							if (factory.isSingleton()) {
								getBean(beanName);
							}
						} else {
							getBean(beanName);
						}
					}
				}
			}
		}
		catch (BeansException ex) {
			try {
				destroySingletons();
			} 
			catch (Throwable ex2) {
				logger.error("Pre-instantiating singletons failed, and couldn't destroy already created singletons", ex2);
			}
			throw ex;
		}
	}
	
	
	//-----------------------------------------------------------------
	// Implementation of ListableBeanFactory interface
	//-----------------------------------------------------------------
	
	@Override
	public int getBeanDefinitionCount() {
		return beanDefinitionMap.size();
	}

	@Override
	public String[] getBeanDefinitionNames() {
		return StringUtils.toStringArray(beanDefinitionNames);
	}

	@Override
	public String[] getBeanDefinitionNames(Class<?> type) {
		List<String> matches = new ArrayList<String>();
		for (String beanName : this.beanDefinitionNames) {
			if (isBeanDefinitionTypeMatch(beanName, type)) {
				matches.add(beanName);
			}
		}
		return StringUtils.toStringArray(matches);
	}

	@Override
	public String[] getBeanNamesForType(Class<?> type) {
		return getBeanNamesForType(type, true, true);
	}

	@Override
	public String[] getBeanNamesForType(Class<?> type,
		boolean includePrototypes, boolean includeFactoryBeans) {
		List<String> result = new ArrayList<String>();
		
		for (String beanName : beanDefinitionNames) {
			RootBeanDefinition rootBeanDefinition = getMergedBeanDefinition(beanName, false);
			
			if (!rootBeanDefinition.isAbstract()) {
				boolean isFactoryBean = rootBeanDefinition.hasBeanClass() && FactoryBean.class.isAssignableFrom(rootBeanDefinition.getBeanClass());
				if (isFactoryBean || rootBeanDefinition.getFactoryBeanName() != null) {
					if (includeFactoryBeans && (includePrototypes || isSingleton(beanName)) && isBeanTypeMatch(beanName, type)) {
						result.add(beanName);
						continue;
					}
					if (!isFactoryBean) {
						continue;
					}
					beanName = FACTORY_BEAN_PREFIX + beanName;
				}
				if ((includePrototypes || rootBeanDefinition.isSingleton()) && isBeanTypeMatch(beanName, type)) {
					result.add(beanName);
				}
			}
		}
		
		String[] singletonNames = getSingletonNames();
		for (String beanName : singletonNames) {
			if (!containsBeanDefinition(beanName)) {
				if (isFactoryBean(beanName)) {
					if (includeFactoryBeans && (includePrototypes || isSingleton(beanName)) 
						&& isBeanTypeMatch(beanName, type)) {
						result.add(beanName);
						continue;
					}
					beanName = FACTORY_BEAN_PREFIX + beanName;
				}
				if (isBeanTypeMatch(beanName, type)) {
					result.add(beanName);
				}
			}
		}
		
		return StringUtils.toStringArray(result);
	}

	@Override
	public Map<String,Object> getBeansOfType(Class<?> type) throws BeansException {
		return getBeansOfType(type, true, true);
	}

	@Override
	public Map<String,Object> getBeansOfType(Class<?> type,
		boolean includePrototypes, boolean includeFactoryBeans) throws BeansException {
		String[] beanNames = getBeanNamesForType(type, includePrototypes, includeFactoryBeans);
		Map<String,Object> result = new LinkedHashMap<String,Object>(beanNames.length);
		for (String beanName : beanNames) {
			try {
				result.put(beanName,getBean(beanName));
			}
			catch (BeanCreationException ex) {
				if (ex.contains(BeanCurrentlyInCreationException.class)) {
					if (logger.isDebugEnabled()) {
						logger.debug(String.format("Ignoring match to currently created bean '%s': %s", beanName, ex.getMessage()));
					}
				} 
				else {
					throw ex;
				}
			}
		}
		return result;
	}

	@Override
	public boolean containsBeanDefinition(String beanName) {
		return this.beanDefinitionMap.containsKey(beanName);
	}
	
	
	//-----------------------------------------------------------------
	// Implementation of BeanDefinitionRegistry interface
	//-----------------------------------------------------------------
	
	@Override
	public void registerBeanDefinition(String beanName, BeanDefinition beanDefinition) throws BeanDefinitionStoreException {
		Assert.hasText(beanName, "Bean name must not be empty");
		Assert.notNull(beanDefinition, "Bean definition must not be null");
		
		if (beanDefinition instanceof AbstractBeanDefinition) {
			try {
				((AbstractBeanDefinition) beanDefinition).validate();
			}
			catch (BeanDefinitionValidationException ex) {
				throw new BeanDefinitionStoreException(beanDefinition.getResourceDescription(), beanName, "Validation of bean definition failed", ex);
			}
		}
		
		Object oldBeanDefinition = this.beanDefinitionMap.get(beanName);
		if (oldBeanDefinition != null) {
			if (!isAllowBeanDefinitionOverriding()) {
				throw new BeanDefinitionStoreException(beanDefinition.getResourceDescription(), beanName, 
					String.format("Cannot register bean definition [%s] for bean '%s': there's already [%s] bound", 
						beanDefinition, beanName, oldBeanDefinition));
			} else {
				if (logger.isInfoEnabled()) {
					logger.info(String.format("Overriding bean definition for bean '%s': replacing [%s] with [%s]", 
						beanName, oldBeanDefinition, beanDefinition));
				}
			}
		}
		else {
			this.beanDefinitionNames.add(beanName);
		}
		this.beanDefinitionMap.put(beanName, beanDefinition);
		
		removeSingleton(beanName);
	}

	//-----------------------------------------------------------------
	// Implementation of superclass abstract class
	//-----------------------------------------------------------------
	
	@Override
	public BeanDefinition getBeanDefinition(String beanName) throws NoSuchBeanDefinitionException {
		BeanDefinition beanDefinition = (BeanDefinition) this.beanDefinitionMap.get(beanName);
		if (beanDefinition == null) {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("No bean named '%s' found in %s", beanName, toString()));
			}
			throw new NoSuchBeanDefinitionException(beanName);
		}
		return beanDefinition;
	}
	
	@Override
	protected Map<String, Object> findMatchingBeans(Class<?> requiredType) throws BeansException {
		return BeanFactoryUtils.beansOfTypeIncludingAncestors(this, requiredType);
	}
	
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer(getClass().getName());
		sb.append(" defining beans [");
		sb.append(StringUtils.arrayToDelimitedString(getBeanDefinitionNames(), ","));
		sb.append("]; ");
		if (getParentBeanFactory() == null) {
			sb.append("root of BeanFactory hierarchy");
		} else {
			sb.append(String.format("parent: %s", getParentBeanFactory()));
		}
		return sb.toString();
	}
	
}
