package org.yuan.study.spring.beans.factory.support;

import org.yuan.study.spring.beans.MutablePropertyValues;
import org.yuan.study.spring.beans.factory.config.AutowireCapableBeanFactory;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.beans.factory.config.ConstructorArgumentValues;

public abstract class AbstractBeanDefinition implements BeanDefinition {
	/**  */
	public static final int AUTOWIRE_NO = 0;
	/**  */
	public static final int AUTOWIRE_AUTODETECT = AutowireCapableBeanFactory.AUTOWIRE_AUTODETECT;
	/**  */
	public static final int AUTOWIRE_BY_NAME = AutowireCapableBeanFactory.AUTOWIRE_BY_NAME;
	/**  */
	public static final int AUTOWIRE_BY_TYPE = AutowireCapableBeanFactory.AUTOWIRE_BY_TYPE;
	/**  */
	public static final int AUTOWIRE_CONSTRUCTOR = AutowireCapableBeanFactory.AUTOWIRE_CONSTRUCTOR;

	/**  */
	public static final int DEPENDENCY_CHECK_NONE = 0;
	/**  */
	public static final int DEPENDENCY_CHECK_OBJECTS = 1;
	/**  */
	public static final int DEPENDENCY_CHECK_SIMPLE = 2;
	/**  */
	public static final int DEPENDENCY_CHECK_ALL = 3;
	
	
	private Object beanClass;
	
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
	
	private String resourceDescription;
	
	
	protected AbstractBeanDefinition() {
		
	}
	
	protected AbstractBeanDefinition(ConstructorArgumentValues cargs, MutablePropertyValues pvs) {
		
	}
	
	//----------------------------------------------------------------
	// Implementation methods
	//----------------------------------------------------------------
	
	
	/**
	 * 
	 * @param other
	 */
	public void overrideFrom(AbstractBeanDefinition other) {
		// TODO
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
	 * 
	 * @throws BeanDefinitionValidationException
	 */
	public void validate() throws BeanDefinitionValidationException {
		// TODO
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
			throw new IllegalStateException(String.format("Bean class name [%s] has not been resolved into an actual Class", this.beanClass));
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
	 * 
	 * @return
	 */
	public int getResolvedAutowireMode() {
		return 0;
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
}
