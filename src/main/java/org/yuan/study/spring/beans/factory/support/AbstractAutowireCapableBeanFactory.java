package org.yuan.study.spring.beans.factory.support;

import java.beans.PropertyDescriptor;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;

import org.yuan.study.spring.beans.BeanUtils;
import org.yuan.study.spring.beans.BeanWrapper;
import org.yuan.study.spring.beans.BeanWrapperImpl;
import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.MutablePropertyValues;
import org.yuan.study.spring.beans.PropertyValue;
import org.yuan.study.spring.beans.PropertyValues;
import org.yuan.study.spring.beans.TypeMismatchException;
import org.yuan.study.spring.beans.factory.BeanCreationException;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.BeanFactoryAware;
import org.yuan.study.spring.beans.factory.BeanNameAware;
import org.yuan.study.spring.beans.factory.InitializingBean;
import org.yuan.study.spring.beans.factory.UnsatisfiedDependencyException;
import org.yuan.study.spring.beans.factory.config.AutowireCapableBeanFactory;
import org.yuan.study.spring.beans.factory.config.BeanPostProcessor;
import org.yuan.study.spring.beans.factory.config.ConstructorArgumentValues;
import org.yuan.study.spring.beans.factory.config.ConstructorArgumentValues.ValueHolder;
import org.yuan.study.spring.beans.factory.config.InstantiationAwareBeanPostProcessor;
import org.yuan.study.spring.util.StringUtils;

public abstract class AbstractAutowireCapableBeanFactory 
	extends AbstractBeanFactory implements AutowireCapableBeanFactory {
	
	private InstantiationStrategy instantiationStrategy = new CglibSubclassingInstantiationStrategy();
	
	/** Whether to automatically try to resolve circular references between beans */
	private boolean allowCircularReferences = true;
	
	/** 
	 * Dependency types to ignore on dependency check and autowire, 
	 * as Set of Class objects: for example, String, Default is none. 
	 */
	private final Set<Class<?>> ignoredDependencyTypes = new HashSet<Class<?>>();
	
	/** 
	 * Dependency interfaces to ignore on dependency check and autowire, 
	 * as Set of Class objects, By default, only the BeanFactory interface is ignored. 
	 */
	private final Set<Class<?>> ignoredDependencyInterfaces = new HashSet<Class<?>>();
	
	
	/**
	 * Create a new AbstractAutowireCapableBeanFactory.
	 */
	public AbstractAutowireCapableBeanFactory() {
		super();
		ignoreDependencyInterface(BeanNameAware.class);
		ignoreDependencyInterface(BeanFactoryAware.class);
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
	 * Set the instantiation strategy to use for creating bean instances.
	 * Default is CglibSubclassingInstantiationStrategy.
	 * @return
	 */
	public InstantiationStrategy getInstantiationStrategy() {
		return instantiationStrategy;
	}

	/**
	 * Return the current instantiation strategy.
	 * @param instantiationStrategy
	 */
	public void setInstantiationStrategy(InstantiationStrategy instantiationStrategy) {
		this.instantiationStrategy = instantiationStrategy;
	}
	
	/**
	 * Return whether to allow circular references between beans - and automatically try to resolve them.
	 * @return
	 */
	public boolean isAllowCircularReferences() {
		return allowCircularReferences;
	}

	/**
	 * Set whether to allow circular references between beans - and automatically try to resolve them.
	 * @param allowCircularReferences
	 */
	public void setAllowCircularReferences(boolean allowCircularReferences) {
		this.allowCircularReferences = allowCircularReferences;
	}

	/**
	 * Return the set of dependency types that will get ignored for autowiring.
	 * @return
	 */
	public Set<Class<?>> getIgnoredDependencyTypes() {
		return ignoredDependencyTypes;
	}

	/**
	 * Return the set of dependency interfaces that will get ignored for autowiring.
	 * @return
	 */
	public Set<Class<?>> getIgnoredDependencyInterfaces() {
		return ignoredDependencyInterfaces;
	}
	
	/**
	 * Ignore the given dependency type of autowiring: 
	 * for example, String. Default is none.
	 * @param type
	 */
	public void ignoreDependencyType(Class<?> type) {
		this.ignoredDependencyTypes.add(type);
	}
	
	/**
	 * Ignore the given dependency interface for autowiring.
	 * @param ifc
	 */
	public void ignoreDependencyInterface(Class<?> ifc) {
		this.ignoredDependencyInterfaces.add(ifc);
	}
	
	/**
	 * Populate the bean instance in the given BeanWrapper with the property values from the bean definition.
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
	 * "autowire constructor" (with constructor arguments by type) behavior.
	 * Also applied if explicit constructor argument values are specified,
	 * matching all remaining arguments with beans from the bean factory.
	 * @param beanName
	 * @param mergedBeanDefinition
	 * @return
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
	 * Apply InstantiationAwareBeanPostProcessors to the specified bean definition, 
	 * invoking their postProcessBeforeInstantiation methods.
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
	 * Give a bean a chance to react now all its properties are set, 
	 * and a chance to know about its owning bean factory.
	 * This means checking whether the bean implements InitializingBean or defines a custom init method, 
	 * and invoking the necessary callback if it does.
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
			((InitializingBean) bean).afterPropertiesSet();
		}
		
		if (mergedBeanDefinition != null && mergedBeanDefinition.getInitMethodName() != null) {
			invokeCustomInitMethod(beanName, bean, mergedBeanDefinition.getInitMethodName(), mergedBeanDefinition.isEnforceDestroyMethod());
		}
	}
	
	/**
	 * Instantiate the given bean using its default constructor.
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
	
	/**
	 * Fill in any missing property values with references to other beans in this factory if autowire is set to "byName".
	 * @param beanName
	 * @param mergedBeanDefinition
	 * @param bw
	 * @param pvs
	 * @throws BeansException
	 */
	protected void autowireByName(String beanName, RootBeanDefinition mergedBeanDefinition, 
		BeanWrapper bw, MutablePropertyValues pvs) throws BeansException {
		String[] propertyNames = unsatisfiedNonSimpleProperties(mergedBeanDefinition, bw);
		for (String propertyName : propertyNames) {
			if (containsBean(propertyName)) {
				Object bean = getBean(propertyName);
				pvs.addPropertyValue(propertyName, bean);
				if (mergedBeanDefinition.isSingleton()) {
					registerDependentBean(propertyName, beanName);
				}
				if (logger.isDebugEnabled()) {
					logger.debug(String.format(
						"Added autowiring by name from bean name '%s' via property '%s' to bean named '%s'", beanName, propertyName, propertyName));
				}
			} 
			else {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Not autowiring property '%s' of bean '%s' by name: no matching bean found", propertyName, beanName));
				}
			}
		}
	}
	
	/**
	 * Abstract method defining "autowire by type" behavior.
	 * This is like PicoContainer default, in which there must be exactly one bean 
	 * of the property type in the bean factory. This makes bean factories simple to configure for small namespaces, 
	 * but doesn't work as well as standard Spring behavior for bigger applications.
	 * @param beanName
	 * @param mergedBeanDefinition
	 * @param bw
	 * @param pvs
	 * @throws BeansException
	 */
	protected void autowireByType(String beanName, RootBeanDefinition mergedBeanDefinition, 
		BeanWrapper bw, MutablePropertyValues pvs) throws BeansException {
		String[] propertyNames = unsatisfiedNonSimpleProperties(mergedBeanDefinition, bw);
		for (String propertyName : propertyNames) {
			Class<?> requiredType = bw.getPropertyDescriptor(propertyName).getPropertyType();
			Map<String,Object> matchingBeans = findMatchingBeans(requiredType);
			if (matchingBeans != null) {
				if (matchingBeans.size() == 1) {
					for (Entry<String,Object> entry : matchingBeans.entrySet()) {
						String autowiredBeanName = entry.getKey();
						Object autowiredBean = entry.getValue();
						pvs.addPropertyValue(propertyName, autowiredBean);
						if (mergedBeanDefinition.isSingleton()) {
							registerDependentBean(autowiredBeanName, beanName);
						}
						if (logger.isDebugEnabled()) {
							logger.debug(String.format(
								"Autowiring by type from bean name '%s' via property '%s' to bean named '%s'", beanName, propertyName, autowiredBeanName));
						}
						break;
					}
				}
				else {
					throw new UnsatisfiedDependencyException(mergedBeanDefinition.getResourceDescription(), beanName, propertyName, 
						String.format("There are '%s' beans of type [%s] available for autowiring by type: %s. "
							+ "There should have been exactly 1 to be able to autowire property '%s' of bean '%s'. "
							+ "Consider using autowiring by name instead.", matchingBeans.size(), requiredType.getName(), matchingBeans.keySet(), propertyName, beanName));
				}
			}
			else {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Not autowiring property '%s' of bean '%s' by type: no matching bean found", propertyName, beanName));
				}
			}
		}
	}
	
	/**
	 * Perform a dependency chck that all properties exposed have been set,
	 * if desired, Dependency checks can be objects (collaborating beans),
	 * simple (primitives and String), or all (both).
	 * @param beanName
	 * @param mergedBeanDefinition
	 * @param bw
	 * @param pvs
	 * @throws UnsatisfiedDependencyException
	 */
	protected void checkDependencies(String beanName, RootBeanDefinition mergedBeanDefinition, 
		BeanWrapper bw, PropertyValues pvs) throws UnsatisfiedDependencyException {
		int dependencyCheck = mergedBeanDefinition.getDependencyCheck();
		if (dependencyCheck == RootBeanDefinition.DEPENDENCY_CHECK_NONE) {
			return;
		}
		
		PropertyDescriptor[] pds = bw.getPropertyDescriptors();
		for (PropertyDescriptor pd : pds) {
			if (pd.getWriteMethod() != null && !isExcludedFromDependencyCheck(pd) && !pvs.contains(pd.getName())) {
				boolean isSimple = BeanUtils.isSimpleProperty(pd.getPropertyType());
				boolean unsatisfied = (dependencyCheck == RootBeanDefinition.DEPENDENCY_CHECK_ALL
					|| (isSimple && dependencyCheck == RootBeanDefinition.DEPENDENCY_CHECK_SIMPLE)
					|| (!isSimple && dependencyCheck == RootBeanDefinition.DEPENDENCY_CHECK_OBJECTS));
				if (unsatisfied) {
					throw new UnsatisfiedDependencyException(mergedBeanDefinition.getResourceDescription(), beanName, pd.getName(), 
						"Set this property value or disable dependency checking for this bean.");
				}
			}
		}
	}
	
	/**
	 * Apply the given property values, resolving any runtime references to other beans in this bean factory. 
	 * Must use deep copy, so we don't permanently modify this property.
	 * @param beanName
	 * @param mergedBeanDefinition
	 * @param bw
	 * @param pvs
	 * @throws BeansException
	 */
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
	
	/**
	 * Return an array of non-simple bean properties that are unsatisfied.
	 * There are probably unsatisfied references to other beans in the factory.
	 * Does not include simple properties like primitives or Strings.
	 * @param mergedBeanDefinition
	 * @param bw
	 * @return
	 */
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
	
	/**
	 * Determine whether the given bean property is excluded from dependency checks.
	 * This implementation excludes properties defined by CGLIB and 
	 * properties whose type matches an ignored dependency type or which
	 * are defined by an ignored dependency interface.
	 * @param pd
	 * @return
	 */
	protected boolean isExcludedFromDependencyCheck(PropertyDescriptor pd) {
		return (AutowireUtils.isExcludedFromDependencyCheck(pd) 
			|| getIgnoredDependencyTypes().contains(pd.getPropertyType()) 
			|| AutowireUtils.isSetterDefinedInInterface(pd, getIgnoredDependencyInterfaces()));
	}
	
	/**
	 * This implementation determines the type matching createBean different creation strategies.
	 */
	protected Class<?> getTypeForFactoryMethod(String beanName, RootBeanDefinition mergedBeanDefinition) {
		if (mergedBeanDefinition.getFactoryBeanName() != null 
			&& mergedBeanDefinition.isSingleton() && !mergedBeanDefinition.isLazyInit()) {
			return getBean(beanName).getClass();
		}
		
		Class<?> factoryClass = null;
		boolean isStatic = true;
		
		if (mergedBeanDefinition.getFactoryBeanName() != null) {
			factoryClass = getType(mergedBeanDefinition.getFactoryBeanName());
			isStatic = false;
		}
		else {
			if (!mergedBeanDefinition.hasBeanClass()) {
				return null;
			}
			factoryClass = mergedBeanDefinition.getBeanClass();
		}
		
		int minNrOfArgs = mergedBeanDefinition.getConstructorArgumentValues().getArgumentCount();
		Method[] candidates = factoryClass.getMethods();
		Set<Class<?>> returnTypes = new HashSet<Class<?>>(1);
		for (Method factoryMethod : candidates) {
			if (Modifier.isStatic(factoryMethod.getModifiers()) == isStatic 
				&& factoryMethod.getName().equals(mergedBeanDefinition.getFactoryMethodName())
				&& factoryMethod.getParameterTypes().length >= minNrOfArgs) {
				returnTypes.add(factoryMethod.getReturnType());
			}
		}
		
		if (returnTypes.size() == 1) {
			return (Class<?>) returnTypes.iterator().next();
		}
		
		return null;
	}
	
	/**
	 * Instantiate the bean using a named factory method. The method may be static, 
	 * if the mergedBeanDefinition parameter specifies a class, rather than a factoryBean,
	 * or an instance variable on a factory object itself configured using Dependency Injection.
	 * 
	 * @param beanName
	 * @param mergedBeanDefinition
	 * @param explicitArgs
	 * @return
	 * @throws BeansException
	 */
	protected BeanWrapper instantiateUsingFactoryMethod(String beanName, RootBeanDefinition mergedBeanDefinition, Object[] explicitArgs) throws BeansException {
		ConstructorArgumentValues cargs = mergedBeanDefinition.getConstructorArgumentValues();
		ConstructorArgumentValues resolvedValues = null;
		
		int minNrOfArgs = 0;
		if (explicitArgs == null) {
			resolvedValues = new ConstructorArgumentValues();
			minNrOfArgs = resolveConstructorArguments(beanName, mergedBeanDefinition, cargs, resolvedValues);
		}
		else {
			minNrOfArgs = explicitArgs.length;
		}
		
		boolean isStatic = true;
		Class<?> factoryClass = null;
		Object factoryBean = null;
		
		if (mergedBeanDefinition.getFactoryBeanName() != null) {
			factoryBean = getBean(mergedBeanDefinition.getFactoryBeanName());
			factoryClass = factoryBean.getClass();
			isStatic = false;
		}
		else {
			factoryClass = mergedBeanDefinition.getBeanClass();
		}
		
		BeanWrapperImpl bw = new BeanWrapperImpl();
		initBeanWrapper(bw);
		
		Method[] candidates = factoryClass.getMethods();
		Method factoryMethodToUse = null;
		Object[] argsToUse = null;
		int minTypeDiffWeight = Integer.MAX_VALUE;
		
		for (int i = 0; i < candidates.length; i++) {
			Method factoryMethod = candidates[i];
			
			if (Modifier.isStatic(factoryMethod.getModifiers()) == isStatic 
				&& factoryMethod.getName().equals(mergedBeanDefinition.getFactoryMethodName())
				&& factoryMethod.getParameterTypes().length >= minNrOfArgs) {
				
				Class<?>[] argTypes = factoryMethod.getParameterTypes();
				ArgumentsHolder args = null;
				
				if (resolvedValues != null) {
					try {
						args = createArgumentArray(beanName, mergedBeanDefinition, resolvedValues, bw, argTypes, "factory method");
					} catch (UnsatisfiedDependencyException ex) {
						if (logger.isDebugEnabled()) {
							logger.debug(String.format("Ignoring factory method [%s] of bean '%s': %s", factoryMethod, beanName, ex.getMessage()));
						}
						if (i == candidates.length - 1 && factoryMethodToUse == null) {
							throw ex;
						}
						else {
							continue;
						}
					}
				}
				else {
					if (argTypes.length != explicitArgs.length) {
						continue;
					}
					args = new ArgumentsHolder(explicitArgs);
				}
				
				int typeDiffWeight = args.getTypeDifferenceWeight(argTypes);
				if (typeDiffWeight < minTypeDiffWeight) {
					factoryMethodToUse = factoryMethod;
					argsToUse = args.arguments;
					minTypeDiffWeight = typeDiffWeight;
				}
			}
		}
		
		if (factoryMethodToUse == null) {
			throw new BeanCreationException(mergedBeanDefinition.getResourceDescription(), beanName, 
				String.format("Cannot find matching factory method '%s' on class [%s]", mergedBeanDefinition.getFactoryMethodName(), factoryClass.getName()));
		}
		
		Object beanInstance = this.instantiationStrategy.instantiate(mergedBeanDefinition, beanName, this, factoryBean, factoryMethodToUse, argsToUse);
		if (beanInstance == null) {
			throw new BeanCreationException(mergedBeanDefinition.getResourceDescription(), beanName, 
				String.format("Factory method '%s' on class [%s] returned null", mergedBeanDefinition.getFactoryMethodName(), factoryClass.getName()));
		}
		
		bw.setWrappedInstance(beanInstance);
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Bean '%s' instantiated via factory method '%s'", beanName, factoryMethodToUse));
		}
		
		return bw;
	}
	
	
	/**
	 * Create an array of arguments to invoke a constructor or factory method,
	 * given the resolved constructor argument values.
	 * @param beanName
	 * @param mergedBeanDefinition
	 * @param resolvedValues
	 * @param bw
	 * @param argTypes
	 * @param methodType
	 * @return
	 * @throws UnsatisfiedDependencyException
	 */
	private ArgumentsHolder createArgumentArray(String beanName, RootBeanDefinition mergedBeanDefinition, 
		ConstructorArgumentValues resolvedValues, BeanWrapperImpl bw, Class<?>[] argTypes, String methodType) throws UnsatisfiedDependencyException {
		ArgumentsHolder args = new ArgumentsHolder(argTypes.length);
		Set<ValueHolder> usedValueHolders = new HashSet<ValueHolder>();
		
		for (int i = 0; i < argTypes.length; i++) {
			ValueHolder valueHolder = resolvedValues.getArgumentValue(i, argTypes[i], usedValueHolders);
			if (valueHolder == null && 
				mergedBeanDefinition.getResolvedAutowireMode() != RootBeanDefinition.AUTOWIRE_CONSTRUCTOR) {
				valueHolder = resolvedValues.getGenericArgumentValue(null, usedValueHolders);
			}
			if (valueHolder != null) {
				usedValueHolders.add(valueHolder);
				args.rawArguments[i] = valueHolder.getValue();
				try {
					args.arguments[i] = doTypeConversionIfNecessary(args.rawArguments[i], argTypes[i], bw);
				}
				catch (TypeMismatchException ex) {
					throw new UnsatisfiedDependencyException(
						mergedBeanDefinition.getResourceDescription(), beanName, i, argTypes[i], 
						String.format("Could not convert %s argument value of type [%s] to required type [%s]: %s", 
							methodType, (valueHolder.getValue() != null ? valueHolder.getValue().getClass().getName() : "null"), argTypes[i].getName(), ex.getMessage()));
				}
			}
			else {
				if (mergedBeanDefinition.getResolvedAutowireMode() != RootBeanDefinition.AUTOWIRE_CONSTRUCTOR) {
					throw new UnsatisfiedDependencyException(
						mergedBeanDefinition.getResourceDescription(), beanName, i, argTypes[i], 
						String.format("Ambiguous %s argument types - did you specify the correct bean references as %s arguments?", methodType, methodType));
				}
				Map<String,Object> matchingBeans = findMatchingBeans(argTypes[i]);
				if (matchingBeans == null || matchingBeans.size() != 1) {
					int matchingBeansCount = (matchingBeans != null ? matchingBeans.size() : 0);
					throw new UnsatisfiedDependencyException(
							mergedBeanDefinition.getResourceDescription(), beanName, i, argTypes[i], 
							String.format("There are %s beans of type [%s] available for autowiring: %s. There should have been exactly 1 to be able to autowire %s of bean '%s'.", 
								matchingBeansCount, argTypes[i].getName(), matchingBeans.keySet(), methodType, beanName));
				}
				for (Entry<String,Object> entry : matchingBeans.entrySet()) {
					String autowiredBeanName = entry.getKey();
					Object autowiredBean = entry.getValue();
					args.rawArguments[i] = autowiredBean;
					args.arguments[i] = autowiredBean;
					if (mergedBeanDefinition.isSingleton()) {
						registerDependentBean(autowiredBeanName, beanName);
					}
					if (logger.isDebugEnabled()) {
						logger.debug(String.format(
							"Autowiring by type from bean name '%s' via %s to bean named '%s'", beanName, methodType, autowiredBeanName));
					}
					break;
				}
			}
		}
		
		return args;
	}
	
	/**
	 * Resolve the constructor arguments for this bean into the resolvedValues object.
	 * This may involve looking up other beans.
	 * This method is also used for handling invocation of static factory methods.
	 * @param beanName
	 * @param mergedBeanDefinition
	 * @param cargs
	 * @param resolvedValues
	 * @return
	 */
	private int resolveConstructorArguments(String beanName, RootBeanDefinition mergedBeanDefinition, 
		ConstructorArgumentValues cargs, ConstructorArgumentValues resolvedValues) {
		BeanDefinitionValueResolver valueResolver = new BeanDefinitionValueResolver(this, beanName, mergedBeanDefinition);
		int minNrOfArgs = cargs.getArgumentCount();
		
		for (Entry<Integer,ValueHolder> entry : cargs.getIndexedArgumentValues().entrySet()) {
			int index = entry.getKey().intValue();
			if (index < 0) {
				throw new BeanCreationException(mergedBeanDefinition.getResourceDescription(), beanName, "Invalid constructor argument index: " + index);
			}
			if (index > minNrOfArgs) {
				minNrOfArgs = index + 1;
			}
			String argName = "constructor argument with index " + index;
			ValueHolder valueHolder = (ValueHolder) entry.getValue();
			Object resolvedValue = valueResolver.resolveValueIfNecessary(argName, valueHolder.getValue());
			resolvedValues.addIndexedArgumentValue(index, resolvedValue, valueHolder.getType());
		}
		
		for (ValueHolder valueHolder : cargs.getGenericArgumentValues()) {
			String argName = "constructor argument";
			Object resolvedValue = valueResolver.resolveValueIfNecessary(argName, valueHolder.getValue());
			resolvedValues.addGenericArgumentValue(resolvedValue, valueHolder.getType());
		}
		
		return minNrOfArgs;
	}
	
	/**
	 * Invoke the specified custom init method on the given bean. Called by invokeInitMethods.
	 * Can be overridden in subclasses for custom resolution of init methods with arguments.
	 * @param beanName
	 * @param bean
	 * @param initMethodName
	 * @param enforceInitMethod
	 * @throws Throwable
	 */
	protected void invokeCustomInitMethod(String beanName, Object bean, String initMethodName, boolean enforceInitMethod) throws Throwable {
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Invoking custom init method '%s' on bean with name '%s'", initMethodName, beanName));
		}
		Method initMethod = BeanUtils.findMethod(bean.getClass(), initMethodName, null);
		if (initMethod == null) {
			if (enforceInitMethod) {
				throw new NoSuchMethodException(String.format("Couldn't find an init method named '%s' on bean with name '%s'", initMethodName, beanName));
			}
			else {
				return;
			}
		}
		if (!Modifier.isPublic(initMethod.getModifiers())) {
			initMethod.setAccessible(true);
		}
		try {
			initMethod.invoke(bean, (Object[]) null);
		}
		catch (InvocationTargetException ex) {
			throw ex.getTargetException();
		}
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
	public void autowireBeanProperties(Object existingBean, int autowireMode, boolean dependencyCheck) throws BeansException {
		if (autowireMode != AUTOWIRE_BY_NAME && autowireMode != AUTOWIRE_BY_TYPE) {
			throw new IllegalArgumentException("Just constants AUTOWIRE_BY_NAME and AUTOWIRE_BY_TYPE allowed");
		}
		
		RootBeanDefinition rootBeanDefinition = new RootBeanDefinition(existingBean.getClass(), autowireMode, dependencyCheck);
		rootBeanDefinition.setSingleton(false);
		populateBean(existingBean.getClass().getName(), rootBeanDefinition, new BeanWrapperImpl(existingBean));
	}
	
	
	//-----------------------------------------------------------------
	// Implementation of AbstractBeanFactory interface
	//-----------------------------------------------------------------

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
	// Abstract method to be implemented by subclasses
	//---------------------------------------------------------------------------------
	
	/**
	 * Find bean instances that match the required type. Called by autowiring.
	 * If a subclass cannot obtain information about names by type,
	 * a corresponding exception should be thrown.
	 * @param requiredType
	 * @return
	 * @throws BeansException
	 */
	protected abstract Map<String,Object> findMatchingBeans(Class<?> requiredType) throws BeansException;
	
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
