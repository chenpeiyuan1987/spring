package org.yuan.study.spring.beans.factory.support;

import java.lang.reflect.Constructor;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

import org.yuan.study.spring.beans.BeanMetadataAttributeAccessor;
import org.yuan.study.spring.beans.MutablePropertyValues;
import org.yuan.study.spring.beans.factory.config.AutowireCapableBeanFactory;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.beans.factory.config.ConstructorArgumentValues;
import org.yuan.study.spring.core.io.DescriptiveResource;
import org.yuan.study.spring.core.io.Resource;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.ObjectUtils;
import org.yuan.study.spring.util.StringUtils;

public abstract class AbstractBeanDefinition extends BeanMetadataAttributeAccessor implements BeanDefinition, Cloneable {
	private static final long serialVersionUID = 1L;

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
	
	private boolean prototype = false;
	
	private boolean lazyInit = false;
	
	private int autowireMode = AUTOWIRE_NO;
	
	private int dependencyCheck = DEPENDENCY_CHECK_NONE;
	
	private String[] dependsOn;
	
	private boolean autowireCandidate = true;
	
	private boolean primary = false;
	
	private final Map<String, AutowireCandidateQualifier> qualifiers = 
		new LinkedHashMap<String, AutowireCandidateQualifier>(0);
	
	private boolean nonPublicAccessAllowed = true;
	
	private boolean lenientConstructorResolution = true;
	
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
	
	private int role = BeanDefinition.ROLE_APPLICATION;
	
	private String description;
	
	private Resource resource;
	
	
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
	 * @param orignial
	 */
	@Deprecated
	protected AbstractBeanDefinition(AbstractBeanDefinition original) {
		this((BeanDefinition) original);
	}
	
	/**
	 * Create a new AbstractBeanDefinition as deep copy of the given bean definition.
	 * @param original
	 */
	protected AbstractBeanDefinition(BeanDefinition original) {
		setParentName(original.getParentName());
		setBeanClassName(original.getBeanClassName());
		setFactoryBeanName(original.getFactoryBeanName());
		setFactoryMethodName(original.getFactoryMethodName());
		setScope(original.getScope());
		setAbstract(original.isAbstract());
		setLazyInit(original.isLazyInit());
		setRole(original.getRole());
		setConstructorArgumentValues(new ConstructorArgumentValues(original.getConstructorArgumentValues()));
		setPropertyValues(new MutablePropertyValues(original.getPropertyValues()));
		setSource(original.getSource());
		copyAttributesFrom(original);
		
		if (original instanceof AbstractBeanDefinition) {
			AbstractBeanDefinition originalAbd = (AbstractBeanDefinition) original;
			if (originalAbd.hasBeanClass()) {
				setBeanClass(originalAbd.getBeanClass());
			}
			setAutowireMode(originalAbd.getAutowireMode());
			setDependencyCheck(originalAbd.getDependencyCheck());
			setDependsOn(originalAbd.getDependsOn());
			setAutowireCandidate(originalAbd.isAutowireCandidate());
			copyQualifiersFrom(originalAbd);
			setPrimary(originalAbd.isPrimary());
			setNonPublicAccessAllowed(originalAbd.isNonPublicAccessAllowed());
			setLenientConstructorResolution(originalAbd.isLenientConstructorResolution());
			setInitMethodName(originalAbd.getInitMethodName());
			setEnforceInitMethod(originalAbd.isEnforceInitMethod());
			setDestroyMethodName(originalAbd.getDestroyMethodName());
			setEnforceDestroyMethod(originalAbd.isEnforceDestroyMethod());
			setMethodOverrides(new MethodOverrides(originalAbd.getMethodOverrides()));
			setSynthetic(originalAbd.isSynthetic());
			setResource(originalAbd.getResource());
		} 
		else {
			setResourceDescription(original.getResourceDescription());
		}
	}
	
	//----------------------------------------------------------------
	// Implementation methods
	//----------------------------------------------------------------
	
	/**
	 * Override settings in this bean definition from the given bean definition.
	 * @param other
	 */
	public void overrideFrom(AbstractBeanDefinition other) {
		overrideFrom((BeanDefinition) other);
	}
	
	/**
	 * Override settings in this bean definition from the given bean definition.
	 * @param other
	 */
	public void overrideFrom(BeanDefinition other) {
		if (StringUtils.hasLength(other.getBeanClassName())) {
			setBeanClassName(other.getBeanClassName());
		}
		if (StringUtils.hasLength(other.getFactoryBeanName())) {
			setFactoryBeanName(other.getFactoryBeanName());
		}
		if (StringUtils.hasLength(other.getFactoryMethodName())) {
			setFactoryMethodName(other.getFactoryMethodName());
		}
		if (StringUtils.hasLength(other.getScope())) {
			setScope(other.getScope());
		}
		
		setAbstract(other.isAbstract());
		setLazyInit(other.isLazyInit());
		setRole(other.getRole());
		getConstructorArgumentValues().addArgumentValues(other.getConstructorArgumentValues());;
		getPropertyValues().addPropertyValues(other.getPropertyValues());
		setSource(other.getSource());
		copyAttributesFrom(other);

		if (other instanceof AbstractBeanDefinition) {
			AbstractBeanDefinition otherAbd = (AbstractBeanDefinition) other;
			if (otherAbd.hasBeanClass()) {
				setBeanClass(otherAbd.getBeanClass());
			}
			if (StringUtils.hasLength(otherAbd.getInitMethodName())) {
				setInitMethodName(otherAbd.getInitMethodName());
				setEnforceInitMethod(otherAbd.isEnforceInitMethod());
			}
			if (StringUtils.hasLength(otherAbd.getDestroyMethodName())) {
				setDestroyMethodName(otherAbd.getDestroyMethodName());
				setEnforceDestroyMethod(otherAbd.isEnforceDestroyMethod());
			}
			
			setAutowireMode(otherAbd.getAutowireMode());
			setAutowireCandidate(otherAbd.isAutowireCandidate());
			copyQualifiersFrom(otherAbd);
			setPrimary(otherAbd.isPrimary());
			setDependencyCheck(otherAbd.getDependencyCheck());
			setDependsOn(otherAbd.getDependsOn());
			setNonPublicAccessAllowed(otherAbd.isNonPublicAccessAllowed());
			setLenientConstructorResolution(otherAbd.isLenientConstructorResolution());
			setMethodOverrides(new MethodOverrides(otherAbd.getMethodOverrides()));
			setSynthetic(otherAbd.isSynthetic());
			setResource(otherAbd.getResource());
		}
		else {
			setResourceDescription(other.getResourceDescription());
		}
	}
	
	/**
	 * Apply the provided default values to this bean.
	 * @param defaults
	 */
	public void applyDefaults(BeanDefinitionDefaults defaults) {
		setLazyInit(defaults.isLazyInit());
		setAutowireMode(defaults.getAutowireMode());
		setDependencyCheck(defaults.getDependencyCheck());
		setInitMethodName(defaults.getInitMethodName());
		setEnforceInitMethod(false);
		setDestroyMethodName(defaults.getDestroyMethodName());
		setEnforceDestroyMethod(false);
	}
	
	/**
	 * Return whether thsi definition specifies a bean class.
	 * @return
	 */
	public boolean hasBeanClass() {
		return (this.beanClass instanceof Class);
	}
	
	/**
	 * Specify the class for this bean.
	 * @param beanClass
	 */
	public void setBeanClass(Class<?> beanClass) {
		this.beanClass = beanClass;
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
	 * Set the role hint for this BeanDefinition.
	 * @param role
	 */
	public void setRole(int role) {
		this.role = role;
	}
	
	/**
	 * Return the resource that this bean definition came from.
	 * @return
	 */
	public Resource getResource() {
		return resource;
	}
	
	/**
	 * Set the resouce that this bean definition came from.
	 * @param resource
	 */
	public void setResource(Resource resource) {
		this.resource = resource;
	}
	
	/**
	 * Specify whether to allow access to non-public constructors and methods,
	 * for the case of externalized metadata pointing to those.
	 * @param nonPublicAccessAllowed
	 */
	public void setNonPublicAccessAllowed(boolean nonPublicAccessAllowed) {
		this.nonPublicAccessAllowed = nonPublicAccessAllowed;
	}
	
	/**
	 * Return whether to allow access to non-public constructors and methods.
	 * @return
	 */
	public boolean isNonPublicAccessAllowed() {
		return nonPublicAccessAllowed;
	}
	
	/**
	 * Specify whether to resolve constructors in lenient mode or to switch to strict resolution.
	 * @param lenientConstructorResolution
	 */
	public void setLenientConstructorResolution(boolean lenientConstructorResolution) {
		this.lenientConstructorResolution = lenientConstructorResolution;
	}
	
	/**
	 * Return whether to resolve constructors in lenient mode or in strict mode.
	 * @return
	 */
	public boolean isLenientConstructorResolution() {
		return lenientConstructorResolution;
	}
	
	/**
	 * Copy the qualifiers from the supplied AbstractBeanDefinition to this bean definition.
	 * @param source
	 */
	public void copyQualifiersFrom(AbstractBeanDefinition source) {
		Assert.notNull(source, "Source must not be null");
		
		qualifiers.putAll(source.qualifiers);
	}
	
	/**
	 * Determine the class of the wrapped bean, resolving it from a
	 * specified class name if necessary.
	 * @param classLoader
	 * @return
	 * @throws ClassNotFoundException
	 */
	public Class<?> resolveBeanClass(ClassLoader classLoader) throws ClassNotFoundException {
		String className = getBeanClassName();
		if (className == null) {
			return null;
		}
		Class<?> resolvedClass = ClassUtils.forName(className, classLoader);
		beanClass = resolvedClass;
		return resolvedClass;
	}
	
	//------------------------------------------------------------------
	// Implementation of BeanDefinition Methods
	//------------------------------------------------------------------

	@Override
	public String getScope() {
		return scope;
	}

	@Override
	public void setScope(String scope) {
		this.scope = scope;
		this.singleton = SCOPE_SINGLETON.equals(scope) || SCOPE_DEFAULT.equals(scope);
		this.prototype = SCOPE_PROTOTYPE.equals(scope);
	}

	@Override
	public boolean isAutowireCandidate() {
		return autowireCandidate;
	}

	@Override
	public void setAutowireCandidate(boolean autowireCandidate) {
		this.autowireCandidate = autowireCandidate;
	}

	@Override
	public boolean isPrimary() {
		return primary;
	}

	@Override
	public void setPrimary(boolean primary) {
		this.primary = primary;
	}

	@Override
	public boolean isPrototype() {
		return prototype;
	}

	@Override
	public int getRole() {
		return role;
	}
	
	@Override
	public String getDescription() {
		return description;
	}

	@Override
	public BeanDefinition getOriginatingBeanDefinition() {
		if (resource instanceof BeanDefinitionResource) {
			return ((BeanDefinitionResource) resource).getBeanDefinition();
		}
		return null;
	}

	@Override
	protected Object clone() {
		return cloneBeanDefinition();
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
	 * Set a description of the resource that this bean definition came from 
	 * (for the purpose of showing context in case of errors).
	 * @param resourceDescription
	 */
	public void setResourceDescription(String resourceDescription) {
		resource = new DescriptiveResource(resourceDescription);
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
		this.scope = (singleton ? SCOPE_SINGLETON : SCOPE_PROTOTYPE);
		this.singleton = singleton;
		this.prototype = !singleton;
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
	
	public String getBeanClassName() {
		if (this.beanClass instanceof Class) {
			return ((Class<?>)this.beanClass).getName();
		}
		else {
			return (String) this.beanClass;
		}
	}
	
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
				"Invalid method override: no method with name '%s' on class [%s]", 
					mo.getMethodName(), getBeanClassName()));
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
	
	/**
	 * Return whether this bean definition is 'synthetic', that is,
	 * not defined by the application itself.
	 * @return
	 */
	public boolean isSynthetic() {
		return synthetic;
	}
	
	/**
	 * Set whether this bean definition is 'synthetic', that is, not defined
	 * by the application itself.
	 * @param synthetic
	 */
	public void setSynthetic(boolean synthetic) {
		this.synthetic = synthetic;
	}
	
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
		return (resource != null ? resource.getDescription() : null);
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
	// Implementation of methods
	//--------------------------------------------------------------
	
	/**
	 * Specify the class for this bean.
	 * @param beanClass
	 */
	public void setBeanClass(Object beanClass) {
		this.beanClass = beanClass;
	}

	/**
	 * Set a human-readable description of this bean definition.
	 * @param description
	 */
	public void setDescription(String description) {
		this.description = description;
	}
	
	/**
	 * Validate this bean definition.
	 * @throws BeanDefinitionValidationException
	 */
	public void validate() throws BeanDefinitionValidationException {
		if (!getMethodOverrides().isEmpty() && getFactoryMethodName() != null) {
			throw new BeanDefinitionValidationException(
				"Cannot combine static factory method with method overrides: "
				+ "the static factory method must create the instance");
		}
		
		if (hasBeanClass()) {
			prepareMethodOverrides();
		}
	}
	
	/**
	 * Validate and prepare the method overrides defined for this bean.
	 * @throws BeanDefinitionValidationException
	 */
	public void prepareMethodOverrides() throws BeanDefinitionValidationException {
		MethodOverrides methodOverrides = getMethodOverrides();
		if (!methodOverrides.isEmpty()) {
			for (MethodOverride mo : methodOverrides.getOverrides()) {
				prepareMethodOverride(mo);
			}
		}
	}
	
	/**
	 * Validate and prepare the given method overrides.
	 * @param mo
	 * @throws BeanDefinitionValidationException
	 */
	protected void prepareMethodOverride(MethodOverride mo) throws BeanDefinitionValidationException {
		int count = ClassUtils.getMethodCountForName(getBeanClass(), mo.getMethodName());
		if (count == 0) {
			throw new BeanDefinitionValidationException(String.format(
				"Invalid method override: no method with name '%s' on class [%s]", 
					mo.getMethodName(), getBeanClassName()));
		} 
		else if (count == 1) {
			mo.setOverloaded(false);
		}
	}
	
	/**
	 * Clone this bean definition.
	 * @return
	 */
	public abstract AbstractBeanDefinition cloneBeanDefinition();
	
	//--------------------------------------------------------------
	// Implementation of Object methods
	//--------------------------------------------------------------
	

	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer("class [");
		sb.append(getBeanClassName()).append("]");
		sb.append("; scope=").append(this.scope);
		sb.append("; abstract=").append(this.abstractFlag);
		sb.append("; lazyInit=").append(this.lazyInit);
		sb.append("; autowireMode=").append(this.autowireMode);
		sb.append("; dependencyCheck=").append(this.dependencyCheck);
		sb.append("; autowireCandidate=").append(this.autowireCandidate);
		sb.append("; primary=").append(this.primary);
		sb.append("; factoryBeanName=").append(this.factoryBeanName);
		sb.append("; factoryMethodName=").append(this.factoryMethodName);
		sb.append("; initMethodName=").append(this.initMethodName);
		sb.append("; destroyMethodName=").append(this.destroyMethodName);
		if (this.resource != null) {
			sb.append("; defined in ").append(this.resource.getDescription());
		}
		return sb.toString();
	}

	@Override
	public int hashCode() {
		int result = ObjectUtils.nullSafeHashCode(getBeanClassName());
		result = 29 * result + ObjectUtils.nullSafeHashCode(scope);
		result = 29 * result + ObjectUtils.nullSafeHashCode(constructorArgumentValues);
		result = 29 * result + ObjectUtils.nullSafeHashCode(propertyValues);
		result = 29 * result + ObjectUtils.nullSafeHashCode(factoryBeanName);
		result = 29 * result + ObjectUtils.nullSafeHashCode(factoryMethodName);
		result = 29 * result + super.hashCode();
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof AbstractBeanDefinition)) {
			return false;
		}
		
		AbstractBeanDefinition other = (AbstractBeanDefinition) obj;
		if (!ObjectUtils.nullSafeEquals(getBeanClassName(), other.getBeanClassName())) {
			return false;
		}
		if (!ObjectUtils.nullSafeEquals(scope, other.scope)) {
			return false;
		}
		if (!ObjectUtils.nullSafeEquals(qualifiers, other.qualifiers)) {
			return false;
		}
		if (!ObjectUtils.nullSafeEquals(constructorArgumentValues, other.constructorArgumentValues)) {
			return false;
		}
		if (!ObjectUtils.nullSafeEquals(propertyValues, other.propertyValues)) {
			return false;
		}
		if (!ObjectUtils.nullSafeEquals(methodOverrides, other.methodOverrides)) {
			return false;
		}
		if (!ObjectUtils.nullSafeEquals(factoryBeanName, other.factoryBeanName)) {
			return false;
		}
		if (!ObjectUtils.nullSafeEquals(factoryMethodName, other.factoryMethodName)) {
			return false;
		}
		if (!ObjectUtils.nullSafeEquals(initMethodName, other.initMethodName)) {
			return false;
		}
		if (!ObjectUtils.nullSafeEquals(destroyMethodName, other.destroyMethodName)) {
			return false;
		}
		if (abstractFlag != other.abstractFlag) {
			return false;
		}
		if (lazyInit != other.lazyInit) {
			return false;
		}
		if (autowireMode != other.autowireMode) {
			return false;
		}
		if (dependencyCheck != other.dependencyCheck) {
			return false;
		}
		if (autowireCandidate != other.autowireCandidate) {
			return false;
		}
		if (primary != other.primary) {
			return false;
		}
		if (nonPublicAccessAllowed != other.nonPublicAccessAllowed) {
			return false;
		}
		if (lenientConstructorResolution != other.lenientConstructorResolution) {
			return false;
		}
		if (enforceInitMethod != other.enforceInitMethod) {
			return false;
		}
		if (enforceDestroyMethod != other.enforceDestroyMethod) {
			return false;
		}
		if (synthetic != other.synthetic) {
			return false;
		}
		if (role != other.role) {
			return false;
		}
		if (!Arrays.equals(dependsOn, other.dependsOn)) {
			return false;
		}
		
		return super.equals(obj);
	}
	
}
