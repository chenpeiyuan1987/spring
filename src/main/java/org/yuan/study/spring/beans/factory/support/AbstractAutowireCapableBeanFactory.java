package org.yuan.study.spring.beans.factory.support;

import java.beans.PropertyDescriptor;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import org.springframework.beans.BeanUtils;
import org.springframework.util.StringUtils;
import org.yuan.study.spring.beans.BeanWrapper;
import org.yuan.study.spring.beans.BeanWrapperImpl;
import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.MutablePropertyValues;
import org.yuan.study.spring.beans.PropertyValues;
import org.yuan.study.spring.beans.factory.BeanCreationException;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.BeanFactoryAware;
import org.yuan.study.spring.beans.factory.BeanNameAware;
import org.yuan.study.spring.beans.factory.config.AutowireCapableBeanFactory;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;

public abstract class AbstractAutowireCapableBeanFactory 
	extends AbstractBeanFactory implements AutowireCapableBeanFactory {
	
	private InstantiationStrategy instantiationStrategy = new CglibSubclassingInstantiationStrategy();
	
	/**  */
	private boolean allowCircularReferences = true;
	
	/**  */
	private final Set<Class<?>> ignoredDependencyTypes = new HashSet<Class<?>>();
	
	/**  */
	private final Set<Class<?>> ignoredDependencyInterfaces = new HashSet<Class<?>>();
	
	
	/**
	 * Create a new AbstractAutowireCapableBeanFactory.
	 */
	public AbstractAutowireCapableBeanFactory() {
		super();
		ignoredDependencyInterface(BeanNameAware.class);
		ignoredDependencyInterface(BeanFactoryAware.class);
	}

	/**
	 * Create a new AbstractAutowireCapableBeanFactory with the given parent.
	 * @param parentBeanFactory
	 */
	public AbstractAutowireCapableBeanFactory(BeanFactory parentBeanFactory) {
		this();
		setParentBeanFactory(parentBeanFactory);
	}
	
	
	//-----------------------------------------------------------------
	// Implementation methods
	//-----------------------------------------------------------------
	
	/**
	 * 
	 * @return
	 */
	public InstantiationStrategy getInstantiationStrategy() {
		return instantiationStrategy;
	}

	/**
	 * 
	 * @param instantiationStrategy
	 */
	public void setInstantiationStrategy(InstantiationStrategy instantiationStrategy) {
		this.instantiationStrategy = instantiationStrategy;
	}
	
	/**
	 * 
	 * @return
	 */
	public boolean isAllowCircularReferences() {
		return allowCircularReferences;
	}

	/**
	 * 
	 * @param allowCircularReferences
	 */
	public void setAllowCircularReferences(boolean allowCircularReferences) {
		this.allowCircularReferences = allowCircularReferences;
	}

	/**
	 * 
	 * @return
	 */
	public Set<Class<?>> getIgnoredDependencyTypes() {
		return ignoredDependencyTypes;
	}

	/**
	 * 
	 * @return
	 */
	public Set<Class<?>> getIgnoredDependencyInterfaces() {
		return ignoredDependencyInterfaces;
	}
	
	/**
	 * 
	 * @param type
	 */
	public void ignoredDependencyType(Class<?> type) {
		this.ignoredDependencyTypes.add(type);
	}
	
	/**
	 * 
	 * @param ifc
	 */
	public void ignoredDependencyInterface(Class<?> ifc) {
		this.ignoredDependencyInterfaces.add(ifc);
	}
	
	/**
	 * 
	 * @throws BeansException
	 */
	protected void populateBean(String beanName, RootBeanDefinition mergedBeanDefinition, BeanWrapper bw) throws BeansException {
		PropertyValues pvs = mergedBeanDefinition.getPropertyValues();
		
		if (mergedBeanDefinition.getResolvedAutowireMode() == RootBeanDefinition.AUTOWIRE_BY_NAME 
			|| mergedBeanDefinition.getResolvedAutowireMode() == RootBeanDefinition.AUTOWIRE_BY_TYPE) {
			MutablePropertyValues mpvs = new MutablePropertyValues(pvs);
			
			if (mergedBeanDefinition.getResolvedAutowireMode() == RootBeanDefinition.AUTOWIRE_BY_NAME) {
				autowireByName(beanName, mergedBeanDefinition, bw, mpvs);
			}
			
			if (mergedBeanDefinition.getResolvedAutowireMode() == RootBeanDefinition.AUTOWIRE_BY_TYPE) {
				autowireByType(beanName, mergedBeanDefinition, bw, mpvs);
			}
			
			pvs = mpvs;
		}
		
		checkDependencies(beanName, mergedBeanDefinition, bw, pvs);
		applyPropertyValues(beanName, mergedBeanDefinition, bw, pvs);
	}
	
	/**
	 * 
	 * @throws BeansException
	 */
	protected BeanWrapper autowireConstructor(String beanName, RootBeanDefinition mergedBeanDefinition) throws BeansException {
		return null;
	}
	
	/**
	 * 
	 * @param beanClass
	 * @param beanName
	 * @return
	 * @throws BeanException
	 */
	protected Object applyBeanPostProcessorsBeforeInstantiation(Class<?> beanClass, String beanName) throws BeansException {
		return null;
	}
	
	/**
	 * 
	 * @param beanName
	 * @param bean
	 * @param mergedBeanDefinition
	 * @throws Throwable
	 */
	protected void invokeInitMethods(String beanName, Object bean, RootBeanDefinition mergedBeanDefinition) throws Throwable {
		
	}
	
	/**
	 * 
	 * @param beanName
	 * @param mergedBeanDefinition
	 * @return
	 * @throws BeansException
	 */
	protected BeanWrapper instantiateBean(String beanName, RootBeanDefinition mergedBeanDefinition) throws BeansException {
		Object beanInstance = getInstantiationStrategy().instantiate(mergedBeanDefinition, beanName, this);
		BeanWrapper bw = new BeanWrapperImpl(beanInstance);
		initBeanWrapper(bw);
		return bw;
	}
	
	protected void autowireByName(String beanName, RootBeanDefinition mergedBeanDefinition, 
		BeanWrapper bw, MutablePropertyValues pvs) throws BeansException {
		// TODO
	}
	
	protected void autowireByType(String beanName, RootBeanDefinition mergedBeanDefinition, 
		BeanWrapper bw, MutablePropertyValues pvs) throws BeansException {
		
	}
	
	protected void checkDependencies(String beanName, RootBeanDefinition mergedBeanDefinition, 
		BeanWrapper bw, PropertyValues pvs) {
		// TODO
		
	}
	
	protected void applyPropertyValues(String beanName, RootBeanDefinition mergedBeanDefinition, 
		BeanWrapper bw, PropertyValues pvs) {
		// TODO
	}
	
	protected String[] unsatisfiedNonSimpleProperties(RootBeanDefinition mergedBeanDefinition, BeanWrapper bw) {
		Set<String> result = new TreeSet<String>();
		PropertyValues pvs = mergedBeanDefinition.getPropertyValues();
		PropertyDescriptor[] pds = bw.getPropertyDescriptors();
		for (PropertyDescriptor pd : pds) {
			if (pd.getWriteMethod() != null && !isExcludedFromDependencyCheck(pd) 
				&& !pvs.contains(pd.getName()) && !BeanUtils.isSimpleProperty(pd.getPropertyType())) {
				result.add(pd.getName());
			}
		}
		return StringUtils.toStringArray(result);
	}
	
	protected boolean isExcludedFromDependencyCheck(PropertyDescriptor pd) {
		// TODO
		return false;
	}
	
	//-----------------------------------------------------------------
	// Implementation of AutowireCapableBeanFactory interface
	//-----------------------------------------------------------------
	
	@Override
	public Object applyBeanPostProcessorsAfterInitialization(Object existingBean, String beanName)
			throws BeansException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Object applyBeanPostProcessorsBeforeInitialization(Object existingBean, String beanName)
			throws BeansException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void applyBeanPropertyValues(Object existingBean, String beanName) throws BeansException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Object autowire(Class<?> beanClass, int autowireMode, boolean dependencyCheck) throws BeansException {
		RootBeanDefinition bd = new RootBeanDefinition(beanClass, autowireMode, dependencyCheck);
		bd.setSingleton(false);
		if (bd.getResolvedAutowireMode() == AUTOWIRE_CONSTRUCTOR) {
			return autowireConstructor(beanClass.getName(), bd).getWrappedInstance();
		}
		else {
			Object bean = getInstantiationStrategy().instantiate(bd, null, this);
			populateBean(beanClass.getName(), bd, new BeanWrapperImpl(bean));
			return bean;
		}
	}

	@Override
	public void autowireBeanProperties(Object existingBean, int autowireMode, boolean dependencyCheck)
			throws BeansException {
		// TODO Auto-generated method stub
		
	}
	
	
	//-----------------------------------------------------------------
	// Implementation of AbstractBeanFactory interface
	//-----------------------------------------------------------------

	@Override
	protected boolean containsBeanDefinition(String beanName) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	protected BeanDefinition getBeanDefinition(String beanName) throws BeansException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Object createBean(String beanName, RootBeanDefinition mergedBeanDefinition, Object[] args)
		throws BeanCreationException {
		
		if (mergedBeanDefinition.getDependsOn() != null) {
			for (String dependOn : mergedBeanDefinition.getDependsOn()) {
				getBean(dependOn);
			}
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Creating instance of bean '%s' with merged definition [%s]", beanName, mergedBeanDefinition));
		}
		
		Object bean = null;
		
		if (mergedBeanDefinition.hasBeanClass()) {
			bean = applyBeanPostProcessorsBeforeInstantiation(mergedBeanDefinition.getBeanClass(), beanName);
			if (bean != null) {
				return bean;
			}
		}
		
		BeanWrapper instanceWrapper = null;
		Object originalBean = null;
		String errorMessage = null;
		
		try {
			errorMessage = "Instantiation of bean failed";
			
			if (mergedBeanDefinition.getFactoryMethodName() != null) {
				instanceWrapper = null;
			}
			else if (mergedBeanDefinition.getResolvedAutowireMode() == RootBeanDefinition.AUTOWIRE_CONSTRUCTOR 
				|| mergedBeanDefinition.hasConstructorArgumentValues()) {
				instanceWrapper = autowireConstructor(beanName, mergedBeanDefinition);
			}
			else {
				instanceWrapper = instantiateBean(beanName, mergedBeanDefinition);
			}
			bean = instanceWrapper.getWrappedInstance();
			
			if (isAllowCircularReferences() && isSingletonCurrentlyInCreation(beanName)) {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format(
						"Eagerly caching bean of type [%s] with name '%s' to allow for resolving potential circular references", 
						bean.getClass().getName(), beanName));
				}
				addSingleton(beanName, bean);
			}
			
			errorMessage = "Initialization of bean failed";
			populateBean(beanName, mergedBeanDefinition, instanceWrapper);
			
			if (bean instanceof BeanNameAware) {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Invoking setBeanName on BeanNameAware bean '%s'", beanName));
				}
				((BeanNameAware) bean).setBeanName(beanName);
			}
			
			if (bean instanceof BeanFactoryAware) {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Invoking setBeanFactory on BeanFactoryAware bean '%s'", beanName));
				}
				((BeanFactoryAware) bean).setBeanFactory(this);
			}
			
			originalBean = bean;
			bean = applyBeanPostProcessorsBeforeInitialization(bean, beanName);
			invokeInitMethods(beanName, bean, mergedBeanDefinition);
			bean = applyBeanPostProcessorsAfterInitialization(bean, beanName);
		}
		catch (BeanCreationException ex) {
			throw ex;
		}
		catch (Throwable ex) {
			throw new BeanCreationException(
				mergedBeanDefinition.getResourceDescription(), beanName, errorMessage, ex);
		}
		
		registerDisposableBeanIfNecessary(beanName, originalBean, mergedBeanDefinition);
		
		return bean;
	}

}
