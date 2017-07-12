package org.yuan.study.spring.beans.factory;

import org.yuan.study.spring.beans.BeansException;

public class NoSuchBeanDefinitionException extends BeansException {
	private static final long serialVersionUID = 1L;
	
	/** Name of the missing bean */
	private String beanName;
	/** Required bean type */
	private Class<?> beanType;
	
	public NoSuchBeanDefinitionException(Class<?> type, String message) {
		super(String.format("No unique bean of type [%s] is defined: %s", type.getName(), message));
		this.beanType = type;
	}
	
	public NoSuchBeanDefinitionException(String name, String message) {
		super(String.format("No bean named '%s' is defined: %s", name, message));
		this.beanName = name;
	}
	
	public NoSuchBeanDefinitionException(String name) {
		super(String.format("No bean named '%s' is defined", name));
		this.beanName = name;
	}

	public String getBeanName() {
		return beanName;
	}

	public Class<?> getBeanType() {
		return beanType;
	}

}
