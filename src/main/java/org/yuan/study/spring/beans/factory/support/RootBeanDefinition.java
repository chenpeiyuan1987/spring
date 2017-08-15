package org.yuan.study.spring.beans.factory.support;

import org.springframework.beans.factory.FactoryBean;
import org.yuan.study.spring.beans.MutablePropertyValues;
import org.yuan.study.spring.beans.factory.config.ConstructorArgumentValues;


public class RootBeanDefinition extends AbstractBeanDefinition {
	
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
	 * Create a new RootBeanDefinition with the given singleton status, 
	 * providing property values.
	 * @param beanClass
	 * @param pvs
	 * @param singleton
	 */
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
		super(rootBeanDefinition);
	}
	

	
	//---------------------------------------------------------
	// Implementation of AbstractBeanDefinition class
	//---------------------------------------------------------
	
	@Override
	public void validate() throws BeanDefinitionValidationException {
		super.validate();
		
		if (hasBeanClass()) {
			if (FactoryBean.class.isAssignableFrom(getBeanClass()) && !isSingleton()) {
				throw new BeanDefinitionValidationException(
					"FactoryBean must be defined as singleton - FactoryBeans themselves are not allowed to be prototypes");
			}
		}
	}

	@Override
	public String toString() {
		return "Root bean: " + super.toString();
	}
}
