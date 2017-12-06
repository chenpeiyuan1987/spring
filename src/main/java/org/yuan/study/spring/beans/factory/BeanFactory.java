package org.yuan.study.spring.beans.factory;

import org.yuan.study.spring.beans.BeansException;

public interface BeanFactory {
	/** Used to dereference a FactoryBean instance and distinguish it from
	 *	beans created by the FactoryBean.
	 */
	String FACTORY_BEAN_PREFIX = "&";
	
	/**
	 * Return an instance, which may be shared or independent, of the specified bean.
	 * @param name
	 * @return
	 */
	Object getBean(String name) throws BeansException;
	
	/**
	 * Return an instance, which may be shared or independent, of the specified bean.
	 * @param name
	 * @param requiredType
	 * @return
	 */
	<T> T getBean(String name, Class<T> requiredType) throws BeansException;
	
	/**
	 * Return the bean instance that uniquely matches the given object type, if any.
	 * @param requiredType
	 * @return
	 * @throws BeansException
	 */
	<T> T getBean(Class<T> requiredType) throws BeansException;
	
	/**
	 * Return an instance, which may be shared or independent, of the specified bean.
	 * @param name
	 * @param args
	 * @return
	 * @throws BeansException
	 */
	Object getBean(String name, Object... args) throws BeansException;
	
	/**
	 * Determine the type of the bean with the given name.
	 * @param name
	 * @return
	 */
	Class<?> getType(String name) throws NoSuchBeanDefinitionException;
	
	/**
	 * Is this bean a shared singleton?
	 * @param name
	 * @return
	 */
	boolean isSingleton(String name) throws NoSuchBeanDefinitionException;
	
	/**
	 * Does this bean factory contain a bean with the given name?
	 * @param name
	 * @return
	 */
	boolean containsBean(String name);
	
	/**
	 * Return the aliases for the given bean name, if defined.
	 * @param name
	 * @return
	 */
	String[] getAliases(String name);
	
	/**
	 * Is this bean a prototype? That is, will always return independent instances?
	 * @param name
	 * @return
	 * @throws NoSuchBeanDefinitionException
	 */
	boolean isPrototype(String name) throws NoSuchBeanDefinitionException;
	
	/**
	 * Check whether the bean with the given name matches the specified type.
	 * @param name
	 * @param targetType
	 * @return
	 * @throws NoSuchBeanDefinitionException
	 */
	boolean isTypeMatch(String name, Class<?> targetType) throws NoSuchBeanDefinitionException;
}
