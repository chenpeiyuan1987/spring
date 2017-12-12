package org.yuan.study.spring.beans.factory.support;

import java.lang.reflect.Constructor;

import org.yuan.study.spring.beans.BeanMetadataAttributeAccessor;
import org.yuan.study.spring.beans.MutablePropertyValues;
import org.yuan.study.spring.beans.factory.config.AutowireCapableBeanFactory;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.beans.factory.config.ConstructorArgumentValues;
import org.yuan.study.spring.util.ClassUtils;

public abstract class AbstractBeanDefinition extends BeanMetadataAttributeAccessor implements BeanDefinition, Cloneable {
	
	/** Constant for the default scope name: "", equivalent to singleton status
	 *	but to be overridden from a parent bean definition (if applicable).
	 */
	public static final String SCOPE_DEFAULT = "";
	
	/** Constant that indicates no autowiring at all */
	public static final int AUTOWIRE_NO = AutowireCapableBeanFactory.AUTOWIRE_NO;
	
	/** Constant that indicates determining an appropriate autowire strategy through introspection of the bean class. */
	@Deprecated
	public static final int AUTOWIRE_AUTODETECT = AutowireCapableBeanFactory.AUTOWIRE_AUTODETECT;
	
	/** Constant that indicates autowiring bean properties by name. */
	public static final int AUTOWIRE_BY_NAME = AutowireCapableBeanFactory.AUTOWIRE_BY_NAME;
	
	/** Constant that indicates autowiring bean properties by type. */
	public static final int AUTOWIRE_BY_TYPE = AutowireCapableBeanFactory.AUTOWIRE_BY_TYPE;
	
	/** Constant that indicates autowiring a constructor. */
	public static final int AUTOWIRE_CONSTRUCTOR = AutowireCapableBeanFactory.AUTOWIRE_CONSTRUCTOR;

	/** Constant that indicates no dependency check at all */
	public static final int DEPENDENCY_CHECK_NONE = 0;
	
	/** Constant that indicates dependency checking for object references. */
	public static final int DEPENDENCY_CHECK_OBJECTS = 1;
	
	/** Constant that indicates dependency checking for "simple" properties. */
	public static final int DEPENDENCY_CHECK_SIMPLE = 2;
	
	/** Constant that indicates dependency checking for all properties. */
	public static final int DEPENDENCY_CHECK_ALL = 3;
	
	
	private Object beanClass;
	
	private String scope = SCOPE_DEFAULT;
	
	private boolean abstractFlag = false;
	
	private boolean singleton = true;
	
	private boolean lazyInit = false;
	
	private int autowireMode = AUTOWIRE_NO;
	
	private int dependencyCheck = DEPENDENCY_CHECK_NONE;
	
	private String[] dependsOn;
	
	private ConstructorArgumentValues constructorArgumentValues;
	
	private MutablePropertyValues propertyValues;
	
	private MethodOverrides methodOverrides = new MethodOverrides();
	
	private String factoryBeanName;
	
	private String factoryMethodName;
	
	private String initMethodName;
	
	private String destroyMethodName;
	
	private boolean enforceInitMethod = true;
	
	private boolean enforceDestroyMethod = true;
	
	private boolean synthetic = false;
	
	private boolean synthetic = false;
	
	private boolean synthetic = false;
	
	private boolean synthetic = false;
	
	private String resourceDescription;
	
	/**
	 * Create a new AbstractBeanDefinition with default settings.
	 */
	protected AbstractBeanDefinition() {
		this(null, null);
	}
	
	/**
	 * Create a new AbstractBeanDefinition with the given constructor argument values and property values.
	 * @param cargs
	 * @param pvs
	 */
	protected AbstractBeanDefinition(ConstructorArgumentValues cargs, MutablePropertyValues pvs) {
		setConstructorArgumentValues(cargs);
		setPropertyValues(pvs);
	}
	
	/**
	 * Create a new AbstractBeanDefinition as deep copy of the given bean definition.
	 * @param original
	 */
	protected AbstractBeanDefinition(AbstractBeanDefinition original) {
		this.beanClass = original.beanClass;
		
		setAbstract(original.isAbstract());
		setSingleton(original.isSingleton());
		setLazyInit(original.isLazyInit());
		setAutowireMode(original.getAutowireMode());
		setDependencyCheck(original.getDependencyCheck());
		setDependsOn(original.getDependsOn());
		
		setConstructorArgumentValues(new ConstructorArgumentValues(original.getConstructorArgumentValues()));
		setPropertyValues(new MutablePropertyValues(original.getPropertyValues()));
		setMethodOverrides(new MethodOverrides(original.getMethodOverrides()));
		
		setFactoryBeanName(original.getFactoryBeanName());
		setFactoryMethodName(original.getFactoryMethodName());
		setInitMethodName(original.getInitMethodName());
		setEnforceInitMethod(original.isEnforceInitMethod());
		setDestroyMethodName(original.getDestroyMethodName());
		setEnforceDestroyMethod(original.isEnforceDestroyMethod());
		
		setResourceDescription(original.getResourceDescription());
	}
	
	
	//----------------------------------------------------------------
	// Implementation methods
	//----------------------------------------------------------------
	
	/**
	 * Override settings in this bean definition from the given bean definition.
	 * @param other
	 */
	public void overrideFrom(AbstractBeanDefinition other) {
		if (other.beanClass != null) {
			this.beanClass = other.beanClass;
		}
		
		setAbstract(other.isAbstract());
		setSingleton(other.isSingleton());
		setLazyInit(other.isLazyInit());
		setAutowireMode(other.getAutowireMode());
		setDependencyCheck(other.getDependencyCheck());
		setDependsOn(other.getDependsOn());
		
		getConstructorArgumentValues().addArgumentValues(other.getConstructorArgumentValues());;
		getPropertyValues().addPropertyValues(other.getPropertyValues());
		getMethodOverrides().addOverrides(other.getMethodOverrides());
		
		if (other.getFactoryBeanName() != null) {
			setFactoryBeanName(other.getFactoryBeanName());
		}
		if (other.getFactoryMethodName() != null) {
			setFactoryMethodName(other.getFactoryMethodName());
		}
		if (other.getInitMethodName() != null) {
			setInitMethodName(other.getInitMethodName());
			setEnforceInitMethod(other.isEnforceInitMethod());
		}
		if (other.getDestroyMethodName() != null) {
			setDestroyMethodName(other.getDestroyMethodName());
			setEnforceDestroyMethod(other.isEnforceDestroyMethod());
		}
		
		setResourceDescription(other.getResourceDescription());
	}
	
	/**
	 * Set if this bean is "abstract", i.e. not meant to be instantiated itself but 
	 * rather just serving as parent for concrete child bean definitions.
	 * @param abstractFlag
	 */
	public void setAbstract(boolean abstractFlag) {
		this.abstractFlag = abstractFlag;
	}

	/**
	 * Return the autowire mode as specified in the bean definition.
	 * @return
	 */
	public int getAutowireMode() {
		return autowireMode;
	}

	/**
	 * Set the autowire mode. This determines whether any automagical detection 
	 * and setting of bean references will happen. Default is AUTOWIRE_NO,
	 * which means there's no autowire.
	 * @param autowireMode
	 */
	public void setAutowireMode(int autowireMode) {
		this.autowireMode = autowireMode;
	}

	/**
	 * Return the dependency check code.
	 * @return
	 */
	public int getDependencyCheck() {
		return dependencyCheck;
	}

	/**
	 * Set the dependency check code.
	 * @param dependencyCheck
	 */
	public void setDependencyCheck(int dependencyCheck) {
		this.dependencyCheck = dependencyCheck;
	}
	
	/**
	 * Specify property values for this bean, if any.
	 * @param propertyValues
	 */
	public void setPropertyValues(MutablePropertyValues propertyValues) {
		this.propertyValues = (propertyValues != null ? propertyValues : new MutablePropertyValues());
	}

	/**
	 * Return information about methods to be overridden by the IoC container.
	 * This will be empty if there are no emthod overrides.
	 * Never return null.
	 * @return
	 */
	public MethodOverrides getMethodOverrides() {
		return methodOverrides;
	}

	/**
	 * Specify method overrides for the bean, if any.
	 * @param methodOverrides
	 */
	public void setMethodOverrides(MethodOverrides methodOverrides) {
		this.methodOverrides = (methodOverrides != null ? methodOverrides : new MethodOverrides());
	}

	/**
	 * Returns the factory bean name, if any.
	 * @return
	 */
	public String getFactoryBeanName() {
		return factoryBeanName;
	}

	/**
	 * Specify the factory bean to use, if any.
	 * @param factoryBeanName
	 */
	public void setFactoryBeanName(String factoryBeanName) {
		this.factoryBeanName = factoryBeanName;
	}

	/**
	 * Return the name of the initializer method.
	 * @return
	 */
	public String getInitMethodName() {
		return initMethodName;
	}

	/**
	 * Set the name of the initializer method. The default is null 
	 * in which case there is no initializer method.
	 * @param initMethodName
	 */
	public void setInitMethodName(String initMethodName) {
		this.initMethodName = initMethodName;
	}

	/**
	 * Indicate whether the configured init method is the default.
	 * @return
	 */
	public boolean isEnforceInitMethod() {
		return enforceInitMethod;
	}

	/**
	 * Specify whether or not the configured init method is the default.
	 * Default value is false.
	 * @param enforceInitMethod
	 */
	public void setEnforceInitMethod(boolean enforceInitMethod) {
		this.enforceInitMethod = enforceInitMethod;
	}

	/**
	 * Set whether this bean should be lazily initialized.
	 * @param lazyInit
	 */
	public void setLazyInit(boolean lazyInit) {
		this.lazyInit = lazyInit;
	}

	/**
	 * Set the names of the beans that this bean depends on being initialized.
	 * The bean factory will guarantee that these beans get initialized before.
	 * @param dependsOn
	 */
	public void setDependsOn(String[] dependsOn) {
		this.dependsOn = dependsOn;
	}

	/**
	 * Specify constructor argument values for this bean.
	 * @param constructorArgumentValues
	 */
	public void setConstructorArgumentValues(ConstructorArgumentValues constructorArgumentValues) {
		this.constructorArgumentValues = 
			(constructorArgumentValues != null ? constructorArgumentValues : new ConstructorArgumentValues());
	}

	/**
	 * Specify a factory method, if any. 
	 * This method will be invoked with constructor arguments, 
	 * or with no arguments if none are specified.
	 * @param factoryMethodName
	 */
	public void setFactoryMethodName(String factoryMethodName) {
		this.factoryMethodName = factoryMethodName;
	}

	/**
	 * Set the name of the destroy method. 
	 * The default is null in which case where is no destroy method.
	 * @param destroyMethodName
	 */
	public void setDestroyMethodName(String destroyMethodName) {
		this.destroyMethodName = destroyMethodName;
	}

	/**
	 * Specify whether or not the configured destroy method is the default.
	 * Default value is false.
	 * @param enforceDestroyMethod
	 */
	public void setEnforceDestroyMethod(boolean enforceDestroyMethod) {
		this.enforceDestroyMethod = enforceDestroyMethod;
	}

	/**
	 * Set a description of the resource that this bean definition came from (for the purpose of showing context in case of errors).
	 * @param resourceDescription
	 */
	public void setResourceDescription(String resourceDescription) {
		this.resourceDescription = resourceDescription;
	}

	/**
	 * Validate this bean definition.
	 * @throws BeanDefinitionValidationException
	 */
	public void validate() throws BeanDefinitionValidationException {
		if (this.lazyInit && !this.singleton) {
			throw new BeanDefinitionValidationException("Lazy initialization is only applicable to singleton beans");
		}
		if (!getMethodOverrides().isEmpty() && getFactoryMethodName() != null) {
			throw new BeanDefinitionValidationException(
				"Cannot combine static factory method with method overrides: the static factory method must create the instance.");
		}
		if (hasBeanClass()) {
			for (MethodOverride methodOverride : getMethodOverrides().getOverrides()) {
				validateMethodOverride(methodOverride);
			}
		}
	}
	
	/**
	 * Return whether thsi definition specifies a bean class.
	 * @return
	 */
	public boolean hasBeanClass() {
		return (this.beanClass instanceof Class);
	}
	
	/**
	 * Return the class of the wrapped bean.
	 * @return
	 * @throws IllegalStateException
	 */
	public Class<?> getBeanClass() throws IllegalStateException {
		if (this.beanClass == null) {
			throw new IllegalStateException("No bean class specified on bean definition");
		}
		if (!(this.beanClass instanceof Class)) {
			throw new IllegalStateException(
				String.format("Bean class name [%s] has not been resolved into an actual Class", this.beanClass));
		}
		return (Class<?>) this.beanClass;
	}
	
	/**
	 * Return a factory method, if any.
	 * @return
	 */
	public String getFactoryMethodName() {
		return this.factoryMethodName;
	}
	
	/**
	 * Return the name of the destroy method.
	 * @return
	 */
	public String getDestroyMethodName() {
		return destroyMethodName;
	}
	
	/**
	 * Indicate whether the configured destroy method is the default.
	 * @return
	 */
	public boolean isEnforceDestroyMethod() {
		return enforceDestroyMethod;
	}
	
	/**
	 * Return the bean names that this bean depends on.
	 * @return
	 */
	public String[] getDependsOn() {
		return dependsOn;
	}
	
	/**
	 * Set if this a Singleton, with a single, shared instance returned on all calls.
	 * In case of "false", the BeanFactory will apply the Prototype design pattern, 
	 * with each caller requesting an instance an instance getting an independent instance.
	 * @param singleton
	 */
	public void setSingleton(boolean singleton) {
		this.singleton = singleton;
	}
	
	/**
	 * Return the resolved autowire code,
	 * (resolving AUTOWIRE_AUTODETECT to AUTOWIRE_CONSTRUCTOR or AUTOWIRE_BY_TYPE).
	 * @return
	 */
	public int getResolvedAutowireMode() {
		if (this.autowireMode == AUTOWIRE_AUTODETECT) {
			Constructor<?>[] constructors = getBeanClass().getConstructors();
			for (Constructor<?> constructor : constructors) {
				if (constructor.getParameterTypes().length == 0) {
					return AUTOWIRE_BY_TYPE;
				}
			}
			return AUTOWIRE_CONSTRUCTOR;
		}
		return this.autowireMode;
	}
	
	/**
	 * Specify the class for this bean.
	 * @param beanClass
	 */
	public void setBeanClass(Class<?> beanClass) {
		this.beanClass = beanClass;
	}
	
	/**
	 * Return the class name of the wrapped bean.
	 * @return
	 */
	public String getBeanClassName() {
		if (this.beanClass instanceof Class) {
			return ((Class<?>)this.beanClass).getName();
		}
		else {
			return (String) this.beanClass;
		}
	}
	
	/**
	 * Specify the class name for this bean.
	 * @param beanClassName
	 */
	public void setBeanClassName(String beanClassName) {
		this.beanClass = beanClassName;
	}
	
	/**
	 * Validate the give
	 * @throws BeanDefinitionValidationException
	 */
	protected void validateMethodOverride(MethodOverride mo) throws BeanDefinitionValidationException {
		int count = ClassUtils.getMethodCountForName(getBeanClass(), mo.getMethodName());
		if (count == 0) {
			throw new BeanDefinitionValidationException(String.format(
				"Invalid method override: no method with name '%s' on class [%s]", mo.getMethodName(), getBeanClassName()));
		}
		else if (count == 1) {
			mo.setOverloaded(false);
		}
	}
	
	/**
	 * Return if there are constructor argument values defined for this bean.
	 * @return
	 */
	public boolean hasConstructorArgumentValues() {
		return !constructorArgumentValues.isEmpty();
	}
	
	public boolean isSynthetic() {
		return synthetic;
	}
	
	public void setSynthetic(boolean synthetic) {
		this.synthetic = synthetic;
	}
	
	//--------------------------------------------------------------
	// Implementation of BeanDefinition interface
	//--------------------------------------------------------------
	
	@Override
	public ConstructorArgumentValues getConstructorArgumentValues() {
		return constructorArgumentValues;
	}

	@Override
	public MutablePropertyValues getPropertyValues() {
		return this.propertyValues;
	}

	@Override
	public String getResourceDescription() {
		return resourceDescription;
	}

	@Override
	public boolean isAbstract() {
		return abstractFlag;
	}

	@Override
	public boolean isLazyInit() {
		return lazyInit;
	}

	@Override
	public boolean isSingleton() {
		return singleton;
	}

	
	//--------------------------------------------------------------
	// Implementation of Object methods
	//--------------------------------------------------------------
	
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer("class [");
		sb.append(getBeanClassName()).append("]");
		sb.append("; abstract=").append(this.abstractFlag);
		sb.append("; singleton=").append(this.singleton);
		sb.append("; lazyInit=").append(this.lazyInit);
		sb.append("; autowire=").append(this.autowireMode);
		sb.append("; dependencyCheck=").append(this.dependencyCheck);
		sb.append("; factoryBeanName=").append(this.factoryBeanName);
		sb.append("; factoryMethodName=").append(this.factoryMethodName);
		sb.append("; initMethodName=").append(this.initMethodName);
		sb.append("; destroyMethodName=").append(this.destroyMethodName);
		if (this.resourceDescription != null) {
			sb.append("; defined in ").append(this.resourceDescription);
		}
		return sb.toString();
	}
}
