package org.yuan.study.spring.beans.factory.support;

import java.beans.PropertyDescriptor;
import java.lang.reflect.Constructor;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.config.InstantiationAwareBeanPostProcessor;
import org.springframework.beans.factory.support.BeanDefinitionValueResolver;
import org.yuan.study.spring.beans.BeanUtils;
import org.yuan.study.spring.beans.BeanWrapper;
import org.yuan.study.spring.beans.BeanWrapperImpl;
import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.MutablePropertyValues;
import org.yuan.study.spring.beans.PropertyValue;
import org.yuan.study.spring.beans.PropertyValues;
import org.yuan.study.spring.beans.factory.BeanCreationException;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.BeanFactoryAware;
import org.yuan.study.spring.beans.factory.BeanNameAware;
import org.yuan.study.spring.beans.factory.UnsatisfiedDependencyException;
import org.yuan.study.spring.beans.factory.config.AutowireCapableBeanFactory;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.beans.factory.config.BeanPostProcessor;
import org.yuan.study.spring.beans.factory.config.ConstructorArgumentValues;
import org.yuan.study.spring.util.StringUtils;

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
		ConstructorArgumentValues cargs = mergedBeanDefinition.getConstructorArgumentValues();
		ConstructorArgumentValues resolvedValues = new ConstructorArgumentValues();
		
		BeanWrapperImpl bw = new BeanWrapperImpl();
		initBeanWrapper(bw);
		
		int minNrOfArgs = 0;
		if (cargs != null) {
			minNrOfArgs = resolveConstructorArguments(beanName, mergedBeanDefinition, cargs, resolvedValues);
		}
		
		Constructor<?>[] candidates = mergedBeanDefinition.getBeanClass().getDeclaredConstructors();
		AutowireUtils.sortConstructors(candidates);
		
		Constructor<?> constructorToUse = null;
		Object[] argsToUse = null;
		int minTypeDiffWeight = Integer.MAX_VALUE;
		
		for (int i = 0; i < candidates.length; i++) {
			Constructor<?> constructor = candidates[i];
			
			if (constructorToUse != null 
				&& constructorToUse.getParameterTypes().length > constructor.getParameterTypes().length) {
				break;
			}
			
			if (constructor.getParameterTypes().length < minNrOfArgs) {
				throw new BeanCreationException(mergedBeanDefinition.getResourceDescription(), beanName, 
					String.format("%s constructor arguments specified but no matching constructor found in bean '%s' (hint: specify index and/or type arguments for simple parameters to avoid type ambiguities)", minNrOfArgs, beanName));
			}
			
			try {
				Class<?>[] argTypes = constructor.getParameterTypes();
				ArgumentsHolder args = createArgumentArray(beanName, mergedBeanDefinition, resolvedValues, bw, argTypes, "constructor");
				
				int typeDiffWeight = args.getTypeDifferenceWeight(argTypes);
				
				if (typeDiffWeight < minTypeDiffWeight) {
					constructorToUse = constructor;
					argsToUse = args.arguments;
					minTypeDiffWeight = typeDiffWeight;
				}
			}
			catch (UnsatisfiedDependencyException ex) {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Ignoring constructor [%s] of bean '%s': %s", constructor, beanName, ex.getMessage()));
				}
				if (i == candidates.length - 1 && constructorToUse == null) {
					throw ex;
				}
				else {
					
				}
			}
		}
		
		if (constructorToUse == null) {
			throw new BeanCreationException(
				mergedBeanDefinition.getResourceDescription(), beanName, "Could not resolve matching constructor");
		}
		
		Object beanInstance = this.instantiationStrategy.instantiate(
			mergedBeanDefinition, beanName, this, constructorToUse, argsToUse);
		bw.setWrappedInstance(beanInstance);
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Bean '%s' instantiated via constructor [%s]", beanName, constructorToUse));
		}
		
		return bw;
	}
	
	/**
	 * 
	 * @param beanClass
	 * @param beanName
	 * @return
	 * @throws BeanException
	 */
	protected Object applyBeanPostProcessorsBeforeInstantiation(Class<?> beanClass, String beanName) throws BeansException {
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Invoking BeanPostProcessors before instantiation of bean '%s'", beanName));
		}
		
		for (BeanPostProcessor beanProcessor : getBeanPostProcessors()) {
			if (beanProcessor instanceof InstantiationAwareBeanPostProcessor) {
				Object result = ((InstantiationAwareBeanPostProcessor) beanProcessor).postProcessBeforeInstantiation(beanClass, beanName);
				if (result != null) {
					return result;
				}
			}
		}
		
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
		if (bean instanceof InitializingBean) {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("Invoking afterPropertiesSet() on bean with name '%s'", beanName) );
			}
			((InitializingBean) bean).
		}
		
		if (mergedBeanDefinition != null && mergedBeanDefinition.get) {
			
		}
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
		BeanWrapper bw, PropertyValues pvs) throws BeansException {
		if (pvs == null) {
			return;
		}
		
		BeanDefinitionValueResolver valueResolver = new BeanDefinitionValueResolver(this, beanName, mergedBeanDefinition);
		
		MutablePropertyValues deepCopy = new MutablePropertyValues();
		PropertyValue[] pvArray = pvs.getPropertyValues();
		for (PropertyValue pv : pvArray) {
			Object resolvedValue = valueResolver.resolveValueIfNecessary(String.format("bean property '%s'", pv.getName()), pv.getValue());
			deepCopy.addPropertyValue(pv.getName(), resolvedValue);
		}
		
		try {
			if (!getCustomEditors().isEmpty()) {
				synchronized (this) {
					bw.setPropertyValues(deepCopy);
				}
			}
			else {
				bw.setPropertyValues(deepCopy);
			}
		}
		catch (BeansException ex) {
			throw new BeanCreationException(mergedBeanDefinition.getResourceDescription(), beanName, "Error setting property values", ex);
		}
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
	
	private ArgumentsHolder createArgumentArray(String beanName, RootBeanDefinition mergedBeanDefinition, 
		ConstructorArgumentValues resolvedValues, BeanWrapperImpl bw, Class<?>[] argTypes, String methodType) throws UnsatisfiedDependencyException {
		// TODO
		return null;
	}
	
	private int resolveConstructorArguments(String beanName, RootBeanDefinition mergedBeanDefinition, 
		ConstructorArgumentValues cargs, ConstructorArgumentValues resolvedValues) {
		// TODO
		return 0;
	}
	
	//-----------------------------------------------------------------
	// Implementation of AutowireCapableBeanFactory interface
	//-----------------------------------------------------------------
	
	@Override
	public Object applyBeanPostProcessorsAfterInitialization(Object existingBean, String beanName)
		throws BeansException {
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Invoking BeanPostProcessors before initialization of bean '%s'", beanName));
		}
		Object result = existingBean;
		for (BeanPostProcessor beanPostProcessor : getBeanPostProcessors()) {
			result = beanPostProcessor.postProcessBeforeInitialization(result, beanName);
			if (result == null) {
				throw new BeanCreationException(beanName, String.format(
					"postProcessBeforeInitialization method of BeanPostProcessor [%s] returned null for bean [%s] with name [%s]", 
					beanPostProcessor, result, beanName));
			}
		}
		return result;
	}

	@Override
	public Object applyBeanPostProcessorsBeforeInitialization(Object existingBean, String beanName)
		throws BeansException {
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Invoking BeanPostProcessors after initialization of bean '%s'", beanName));
		}
		Object result = existingBean;
		for (BeanPostProcessor beanPostProcessor : getBeanPostProcessors()) {
			result = beanPostProcessor.postProcessAfterInitialization(result, beanName);
			if (result == null) {
				throw new BeanCreationException(beanName, String.format(
					"postProcessAfterInitialization method of BeanPostProcessor [%s] returned null for bean [%s] with name [%s]", 
					beanPostProcessor, result, beanName));
			}
		}
		return result;
	}

	@Override
	public void applyBeanPropertyValues(Object existingBean, String beanName) throws BeansException {
		RootBeanDefinition bd = getMergedBeanDefinition(beanName, true);
		BeanWrapper bw = new BeanWrapperImpl(existingBean);
		initBeanWrapper(bw);
		applyPropertyValues(beanName, bd, bw, bd.getPropertyValues());
		
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

	//---------------------------------------------------------------------------------
	// Private inner class for internal use
	//---------------------------------------------------------------------------------
	
	private static class ArgumentsHolder {
		
		public Object rawArguments[];
		
		public Object arguments[];

		public ArgumentsHolder(int size) {
			this.rawArguments = new Object[size];
			this.arguments = new Object[size];
		}
		
		public ArgumentsHolder(Object[] args) {
			this.rawArguments = args;
			this.arguments = args;
		}
		
		public int getTypeDifferenceWeight(Class<?>[] argTypes) {
			int typeDiffWeight = AutowireUtils.getTypeDifferenceWeight(argTypes, this.arguments);
			int rawTypeDiffWeight = AutowireUtils.getTypeDifferenceWeight(argTypes, this.rawArguments) - 1024;
			return (rawTypeDiffWeight < typeDiffWeight ? rawTypeDiffWeight : typeDiffWeight);
		}
	}
}
