package org.yuan.study.spring.beans;

public class NotWritablePropertyException extends InvalidPropertyException {
	private static final long serialVersionUID = 1L;

	/**
	 * Create a new NotWritablePropertyException.
	 * @param beanClass
	 * @param propertyName
	 * @param message
	 * @param ex
	 */
	public NotWritablePropertyException(Class<?> beanClass, String propertyName, String message, Throwable ex) {
		super(beanClass, propertyName, message, ex);
	}
	
	/**
	 * Create a new NotWritablePropertyException.
	 * @param beanClass
	 * @param propertyName
	 */
	public NotWritablePropertyException(Class<?> beanClass, String propertyName) {
		super(beanClass, propertyName, String.format("Bean property '%s' is not writable or has an invalid setter method: "
			+ "Does the parameter type of the setter match the return type of the getter?", propertyName));
	}

}
