package org.yuan.study.spring.beans.factory;

import org.yuan.study.spring.beans.BeansException;

public class NoSuchBeanDefinitionException extends BeansException {
	private static final long serialVersionUID = 1L;
	
	/** Name of the missing bean */
	private String beanName;
	/** Required bean type */
	private Class<?> beanType;
	
	/**
	 * Create a new NoSuchBeanDefinitionException.
	 * @param type
	 * @param message
	 */
	public NoSuchBeanDefinitionException(Class<?> type, String message) {
		super(String.format("No unique bean of type [%s] is defined: %s", type.getName(), message));
		this.beanType = type;
	}
	
	/**
	 * Create a new NoSuchBeanDefinitionException.
	 * @param name
	 * @param message
	 */
	public NoSuchBeanDefinitionException(String name, String message) {
		super(String.format("No bean named '%s' is defined: %s", name, message));
		this.beanName = name;
	}
	
	/**
	 * Create a new NoSuchBeanDefinitionException.
	 * @param name
	 */
	public NoSuchBeanDefinitionException(String name) {
		super(String.format("No bean named '%s' is defined", name));
		this.beanName = name;
	}

	/**
	 * Return the name of the missing bean, 
	 * if it was a lookup by name that failed.
	 * @return
	 */
	public String getBeanName() {
		return beanName;
	}

	/**
	 * Return the required type of bean, 
	 * if it was a lookup by type that failed.
	 * @return
	 */
	public Class<?> getBeanType() {
		return beanType;
	}

}
