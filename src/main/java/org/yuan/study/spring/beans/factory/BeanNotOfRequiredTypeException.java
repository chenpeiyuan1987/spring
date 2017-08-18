package org.yuan.study.spring.beans.factory;

import org.yuan.study.spring.beans.BeansException;

public class BeanNotOfRequiredTypeException extends BeansException {
	private static final long serialVersionUID = 1L;
	
	/** The name of the instance that was of the wrong type */
	private String beanName;
	/** The required type */
	private Class<?> actualType;
	/** The offending type */
	private Class<?> requiredType;
	
	/**
	 * Create a new BeanNotOfRequiredTypeException.
	 * @param beanName
	 * @param requiredType
	 * @param actualType
	 */
	public BeanNotOfRequiredTypeException(String beanName,
		Class<?> requiredType, Class<?> actualType) {
		super(String.format("Bean named '%s' must be of type [%s], but was actually of type [%s]", 
			beanName, requiredType.getClass(), actualType.getClass()));
		this.beanName = beanName;
		this.actualType = actualType;
		this.requiredType = requiredType;
	}

	/**
	 * Return the name of the instance that was of the wrong type.
	 * @return
	 */
	public String getBeanName() {
		return beanName;
	}

	/**
	 * Return the actual type of the instance found.
	 * @return
	 */
	public Class<?> getActualType() {
		return actualType;
	}

	/**
	 * Return the required type for the bean.
	 * @return
	 */
	public Class<?> getRequiredType() {
		return requiredType;
	}
	
}
