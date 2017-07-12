package org.yuan.study.spring.beans.factory;

import org.yuan.study.spring.beans.BeansException;

public interface BeanFactory {
	/**  */
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
	Object getBean(String name, Class<?> requiredType) throws BeansException;
	
	/**
	 * Determine the type of the bean with the given name.
	 * @param name
	 * @return
	 */
	Class<?> getType(String name);
	
	/**
	 * Is this bean a shared singleton?
	 * @param name
	 * @return
	 */
	boolean isSingleton(String name);
	
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
}
