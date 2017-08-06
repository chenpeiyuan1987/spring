package org.yuan.study.spring.beans;

public class NullValueInNestedPathException extends InvalidPropertyException {
	private static final long serialVersionUID = 1L;

	/**
	 * Create a new NullValueInNestedPathException
	 * @param beanClass
	 * @param propertyName
	 */
	public NullValueInNestedPathException(Class<?> beanClass, String propertyName) {
		super(beanClass, propertyName, String.format("Value of nested property '%s' is null", propertyName));
	}
	
	/**
	 * Create a new NullValueInNestedPathException
	 * @param beanClass
	 * @param propertyName
	 * @param message
	 */
	public NullValueInNestedPathException(Class<?> beanClass, String propertyName, String message) {
		super(beanClass, propertyName, message);
	}
	
}
