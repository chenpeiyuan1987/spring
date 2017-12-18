package org.yuan.study.spring.beans.factory.support;

import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.yuan.study.spring.beans.MutablePropertyValues;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.beans.factory.config.BeanDefinitionHolder;
import org.yuan.study.spring.beans.factory.config.ConstructorArgumentValues;
import org.yuan.study.spring.util.Assert;


public class RootBeanDefinition extends AbstractBeanDefinition {
	
	private final Set<Member> externallyManagedConfigMembers = Collections.synchronizedSet(new HashSet<Member>(0));
	
	private final Set<String> externallyManagedInitMethods = Collections.synchronizedSet(new HashSet<String>(0));
	
	private final Set<String> externallyManagedDestroyMethods = Collections.synchronizedSet(new HashSet<String>(0));
	
	private BeanDefinitionHolder decoratedDefinition;
	
	boolean isFactoryMethodUnique;
	
	Object resolvedConstructorOrFactoryMethod;
	
	boolean constructorArgumentsResolved = false;
	
	Object[] resolvedConstructorArguments;
	
	Object[] preparedConstructorArguments;
	
	final Object constructorArgumentLock = new Object();
	
	volatile Boolean beforeInstantiationResolved;
	
	boolean postProcessed = false;
	
	final Object postProcessingLock = new Object();
	
	/**
	 * Create a new RootBeanDefinition, 
	 * to be configured through its bean properties and configuration methods.
	 */
	public RootBeanDefinition() {
		super();
	}

	/**
	 * Create a new RootBeanDefinition for a singleton.
	 * @param beanClass
	 */
	public RootBeanDefinition(Class<?> beanClass) {
		super();
		setBeanClass(beanClass);
	}
	
	/**
	 * Create a new RootBeanDefinition with the given singleton status.
	 * @param beanClass
	 * @param singleton
	 */
	@Deprecated
	public RootBeanDefinition(Class<?> beanClass, boolean singleton) {
		super();
		setBeanClass(beanClass);
		setSingleton(singleton);
	}
	
	/**
	 * Create a new RootBeanDefinition for a singleton, 
	 * using the given autowire mode.
	 * @param beanClass
	 * @param autowireMode
	 */
	@Deprecated
	public RootBeanDefinition(Class<?> beanClass, int autowireMode) {
		super();
		setBeanClass(beanClass);
		setAutowireMode(autowireMode);
	}
	
	/**
	 * Create a new RootBeanDefinition for a singleton, 
	 * using the given autowire mode.
	 * @param beanClass
	 * @param autowireMode
	 * @param dependencyCheck
	 */
	public RootBeanDefinition(Class<?> beanClass, int autowireMode, boolean dependencyCheck) {
		super();
		setBeanClass(beanClass);
		setAutowireMode(autowireMode);
		if (dependencyCheck && getResolvedAutowireMode() != AUTOWIRE_CONSTRUCTOR) {
			setDependencyCheck(RootBeanDefinition.DEPENDENCY_CHECK_OBJECTS);
		}
	}
	
	/**
	 * Create a new RootBeanDefinition for a singleton,
	 * providing property values.
	 * @param beanClass
	 * @param pvs
	 */
	@Deprecated
	public RootBeanDefinition(Class<?> beanClass, MutablePropertyValues pvs) {
		super(null, pvs);
		setBeanClass(beanClass);
	}
	
	/**
	 * Create a new RootBeanDefinition with the given singleton status, 
	 * providing property values.
	 * @param beanClass
	 * @param pvs
	 * @param singleton
	 */
	@Deprecated
	public RootBeanDefinition(Class<?> beanClass, MutablePropertyValues pvs, boolean singleton) {
		super(null, pvs);
		setBeanClass(beanClass);
		setSingleton(singleton);
	}

	/**
	 * Create a new RootBeanDefinition for a singleton, 
	 * providing constructor arguments and property values.
	 * @param beanClass
	 * @param cargs
	 * @param pvs
	 */
	public RootBeanDefinition(Class<?> beanClass, ConstructorArgumentValues cargs, MutablePropertyValues pvs) {
		super(cargs, pvs);
		setBeanClass(beanClass);
	}
	
	/**
	 * Create a new RootBeanDefinition for a singleton,
	 * providing constructor arguments and property values.
	 * @param beanClassName
	 */
	public RootBeanDefinition(String beanClassName) {
		setBeanClassName(beanClassName);
	}
	
	/**
	 * Create a new RootBeanDefinition for a singleton, 
	 * providing constructor arguments and property values.
	 * @param beanClassName
	 * @param cargs
	 * @param pvs
	 */
	public RootBeanDefinition(String beanClassName, ConstructorArgumentValues cargs, MutablePropertyValues pvs) {
		super(cargs, pvs);
		setBeanClassName(beanClassName);
	}

	/**
	 * Create a new RootBeanDefinition as deep copy of the given bean definition.
	 * @param rootBeanDefinition
	 */
	public RootBeanDefinition(RootBeanDefinition rootBeanDefinition) {
		this((BeanDefinition)rootBeanDefinition);
	}

	/**
	 * Create a new RootBeanDefinition as deep copy of the given
	 * bean definition.
	 * @param original
	 */
	RootBeanDefinition(BeanDefinition original) {
		super(original);
		if (original instanceof RootBeanDefinition) {
			RootBeanDefinition originalRbd = (RootBeanDefinition)original;
			decoratedDefinition = originalRbd.decoratedDefinition;
			isFactoryMethodUnique = originalRbd.isFactoryMethodUnique;
		}
	}
	
	//---------------------------------------------------------
	// Implementation of AbstractBeanDefinition class
	//---------------------------------------------------------
	
	public void registerExternallyManagedConfigMember(Member configMember) {
		externallyManagedConfigMembers.add(configMember);
	}
	
	public boolean isExternallyManagedConfigMember(Member configMember) {
		return externallyManagedConfigMembers.contains(configMember);
	}
	
	public void registerExternallyManagedInitMethod(String initMethod) {
		externallyManagedInitMethods.add(initMethod);
	}
	
	public boolean isExternallyManagedInitMethod(String initMethod) {
		return externallyManagedInitMethods.contains(initMethod);
	}
	
	public void registerExternallyManagedDestroyMethod(String destroyMethod) {
		externallyManagedDestroyMethods.add(destroyMethod);
	}
	
	public boolean isExternallyManagedDestroyMethod(String destroyMethod) {
		return externallyManagedDestroyMethods.contains(destroyMethod);
	}
	
	public BeanDefinitionHolder getDecoratedDefinition() {
		return decoratedDefinition;
	}

	public void setDecoratedDefinition(BeanDefinitionHolder decoratedDefinition) {
		this.decoratedDefinition = decoratedDefinition;
	}
	
	/**
	 * Specify a factory method name that refers to a non-overloaded method.
	 * @param name
	 */
	public void setUniqueFactoryMethodName(String name) {
		Assert.hasText(name, "Factory method name must not be empty");
		setFactoryMethodName(name);
		isFactoryMethodUnique = true;
	}
	
	/**
	 * Check whether the given candidate qualifies as a factory method.
	 * @param candidate
	 * @return
	 */
	public boolean isFactoryMethod(Method candidate) {
		return candidate != null && candidate.getName().equals(getFactoryMethodName());
	}
	
	/**
	 * Return the resolved factory method as a Java Method object, if available.
	 * @return
	 */
	public Method getResolvedFactoryMethod() {
		synchronized (constructorArgumentLock) {
			Object candidate = resolvedConstructorOrFactoryMethod;
			return candidate instanceof Method ? (Method) candidate : null;
		}
	}
	
	@Override
	public String getParentName() {
		return null;
	}

	@Override
	public void setParentName(String parentName) {
		if (parentName != null) {
			throw new IllegalArgumentException(
				"Root bean cannot be changed into a child bean with parent reference");
		}
	}

	@Override
	public RootBeanDefinition cloneBeanDefinition() {
		return new RootBeanDefinition(this);
	}

	@Override
	public boolean equals(Object other) {
		return this == other || (other instanceof RootBeanDefinition && super.equals(other));
	}
	
	@Override
	public String toString() {
		return "Root bean: " + super.toString();
	}

}
