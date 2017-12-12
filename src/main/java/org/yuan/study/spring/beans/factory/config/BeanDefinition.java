package org.yuan.study.spring.beans.factory.config;

import org.yuan.study.spring.beans.BeanMetadataElement;
import org.yuan.study.spring.beans.MutablePropertyValues;
import org.yuan.study.spring.core.AttributeAccessor;

public interface BeanDefinition extends AttributeAccessor, BeanMetadataElement {
	
	String SCOPE_SINGLETON = ConfigurableBeanFactory.SCOPE_SINGLETON;
	
	String SCOPE_PROTOTYPE = ConfigurableBeanFactory.SCOPE_PROTOTYPE;
	
	int ROLE_APPLICATION = 0;
	
	int ROLE_SUPPORT = 1;
	
	/**
	 * Role hint indicating that a 'BeanDefinition' is providing an entire
	 */
	int ROLE_INFRASTRUCTURE = 2;
	
	/**
	 * Return the name of the parent definition of this bean definition, if any.
	 * @return
	 */
	String getParentName();
	
	/**
	 * Set the name of the parent definition of this bean definition, if any.
	 * @param parentName
	 */
	void setParentName(String parentName);
	
	/**
	 * Return the current bean class name of this bean definition.
	 * @return
	 */
	String getBeanClassName();
	
	/**
	 * Override the bean class name of this bean definition.
	 * @param beanClassName
	 */
	void setBeanClassName(String beanClassName);
	
	/**
	 * Return the factory bean name, if any.
	 * @return
	 */
	String getFactoryBeanName();
	
	/**
	 * Specify the factory bean to use, if any.
	 * @param factoryBeanName
	 */
	void setFactoryBeanName(String factoryBeanName);
	
	/**
	 * Return a factory method, if any.
	 * @return
	 */
	String getFactoryMethodName();
	
	/**
	 * Specify a factory method, if any. This method will be invoked with
	 * constructor arguments, or with no arguments if none are specified.
	 * @param factoryMethodName
	 */
	void setFactoryMethodName(String factoryMethodName);
	
	/**
	 * Return the name of the current target scope for this bean,
	 * or null if not known yet.
	 * @return
	 */
	String getScope();
	
	/**
	 * Override the target scope of this bean, specifying a new scope name.
	 * @param scope
	 */
	void setScope(String scope);
	
	/**
	 * Return whether this bean should be lazily initialized,
	 * i.e. not eagerly instantiated on startup. 
	 * Only applicable to a singleton bean.
	 * @return
	 */
	boolean isLazyInit();
	
	/**
	 * Set whether this bean should be lazily initialized.
	 * @param lazyInit
	 */
	void setLazyInit(boolean lazyInit);
	
	/**
	 * Return the bean namees that this bean depends on.
	 * @return
	 */
	String[] getDependsOn();
	
	/**
	 * Set the names of the beans that this bean depends on being initialized.
	 * @param dependsOn
	 */
	void setDependsOn(String[] dependsOn);
	
	/**
	 * Set whether this bean is a candidate for getting autowired into some other bean.
	 * @return
	 */
	boolean isAutowireCandidate();
	
	/**
	 * Set whether this bean is a candidate for getting autowired into some other bean.
	 * @param autowireCandidate
	 */
	void setAutowireCandidate(boolean autowireCandidate);
	
	/**
	 * Return whether this bean is a primary autowire candidate.
	 * @return
	 */
	boolean isPrimary();
	
	/**
	 * Set whether this bean is a primary autowire candidate.
	 * @param primary
	 */
	void setPrimary(boolean primary);
	
	/**
	 * Return the constructor argument values for this bean, if any.
	 * Can be modified during bean factory post-processing.
	 * @return
	 */
	ConstructorArgumentValues getConstructorArgumentValues();
	
	/**
	 * Return the PropertyValues to be applied to a new instance of the bean, if any.
	 * Can be modified during bean factory post-processing.
	 * @return
	 */
	MutablePropertyValues getPropertyValues();
	
	/**
	 * Return whether this a Singleton, with a single, 
	 * shared instance returned on all calls.
	 * @return
	 */
	boolean isSingleton();
	
	/**
	 * Return whether this a 'Prototype', with an independent instance
	 * returned for each call.
	 * @return
	 */
	boolean isPrototype();
	
	/**
	 * Return whether this bean is "abstract", 
	 * i.e. not meant to be instantiated.
	 * @return
	 */
	boolean isAbstract();
	
	/**
	 * Get the role hint for this BeanDefinition. The role hint
	 * provides tools with an indication of the importance of a 
	 * particular BeanDefinition.
	 * @return
	 */
	int getRole();
	
	/**
	 * Return a human-readable description of this bean definition.
	 * @return
	 */
	String getDescription();
	
	/**
	 * Return a description of the resource that this bean definition 
	 * came from (for the purpose of showing context in case of errors).
	 * @return
	 */
	String getResourceDescription();
	
	/**
	 * Return the originating BeanDefinition, or null if none.
	 * @return
	 */
	BeanDefinition getOriginatingBeanDefinition();
}
