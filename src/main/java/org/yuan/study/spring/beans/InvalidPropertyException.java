package org.yuan.study.spring.beans;

public class InvalidPropertyException extends FatalBeanException {
	private static final long serialVersionUID = 1L;
	
	private final Class<?> beanClass;
	
	private final String propertyName;

	/**
	 * Create a new InvalidPropertyException.
	 * @param beanClass
	 * @param propertyName
	 * @param message
	 * @param ex
	 */
	public InvalidPropertyException(Class<?> beanClass, String propertyName, String message, Throwable ex) {
		super(String.format("Invalid property '%s' of bean class [%s]: %s", propertyName, beanClass.getName(), message), ex);
		this.beanClass = beanClass;
		this.propertyName = propertyName;
	}
	
	/**
	 * Create a new InvalidPropertyException.
	 * @param beanClass
	 * @param propertyName
	 * @param message
	 */
	public InvalidPropertyException(Class<?> beanClass, String propertyName, String message) {
		this(beanClass, propertyName, message, null);
	}

	/**
	 * Return the offending bean class.
	 * @return
	 */
	public Class<?> getBeanClass() {
		return beanClass;
	}

	/**
	 * Return the name of the offending property.
	 * @return
	 */
	public String getPropertyName() {
		return propertyName;
	}

}
