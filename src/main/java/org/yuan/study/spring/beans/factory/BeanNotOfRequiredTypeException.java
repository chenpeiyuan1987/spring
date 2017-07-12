package org.yuan.study.spring.beans.factory;

import org.yuan.study.spring.beans.BeansException;

public class BeanNotOfRequiredTypeException extends BeansException {
	private static final long serialVersionUID = 1L;
	
	/**  */
	private String beanName;
	/**  */
	private Class<?> actualType;
	/**  */
	private Class<?> requiredType;
	
	public BeanNotOfRequiredTypeException(String beanName,
		Class<?> requiredType, Class<?> actualType) {
		super(String.format("Bean named '%s' must be of type [%s], but was actually of type [%s]", 
			beanName, requiredType.getClass(), actualType.getClass()));
		this.beanName = beanName;
		this.actualType = actualType;
		this.requiredType = requiredType;
	}

	public String getBeanName() {
		return beanName;
	}

	public Class<?> getActualType() {
		return actualType;
	}

	public Class<?> getRequiredType() {
		return requiredType;
	}
	
}
