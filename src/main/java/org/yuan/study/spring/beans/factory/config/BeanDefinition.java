package org.yuan.study.spring.beans.factory.config;

import org.yuan.study.spring.beans.MutablePropertyValues;

public interface BeanDefinition {
	
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
	 * Return a description of the resource that this bean definition 
	 * came from (for the purpose of showing context in case of errors).
	 * @return
	 */
	String getResourceDescription();
	
	/**
	 * Return whether this bean is "abstract", 
	 * i.e. not meant to be instantiated.
	 * @return
	 */
	boolean isAbstract();
	
	/**
	 * Return whether this bean should be lazily initialized,
	 * i.e. not eagerly instantiated on startup. 
	 * Only applicable to a singleton bean.
	 * @return
	 */
	boolean isLazyInit();
	
	/**
	 * Return whether this a Singleton, with a single, 
	 * shared instance returned on all calls.
	 * @return
	 */
	boolean isSingleton();
}
