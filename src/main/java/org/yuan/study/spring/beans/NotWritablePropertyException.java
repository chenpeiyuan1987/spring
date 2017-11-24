package org.yuan.study.spring.beans;

public class NotWritablePropertyException extends InvalidPropertyException {
	private static final long serialVersionUID = 1L;
	
	private String[] possibleMatches = null;

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
	 * @param message
	 * @param ex
	 */
	public NotWritablePropertyException(Class<?> beanClass, String propertyName, String message) {
		super(beanClass, propertyName, message);
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

	/**
	 * Create a new NotWritablePropertyException.
	 * @param beanClass
	 * @param propertyName
	 */
	public NotWritablePropertyException(Class<?> beanClass, String propertyName, String msg, String[] possibleMatches) {
		super(beanClass, propertyName, msg);
		this.possibleMatches = possibleMatches;
	}

	/**
	 * Return suggestions for actual bean property names that closely match
	 * the invalid property name, if any.
	 * @return
	 */
	public String[] getPossibleMatches() {
		return possibleMatches;
	}
	
}
