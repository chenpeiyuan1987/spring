package org.yuan.study.spring.beans;

public class NotReadablePropertyException extends InvalidPropertyException {
	private static final long serialVersionUID = 1L;

	/**
	 * Create a new NotReadablePropertyException.
	 * @param beanClass
	 * @param propertyName
	 */
	public NotReadablePropertyException(Class<?> beanClass, String propertyName) {
		super(beanClass, propertyName, String.format("Bean property '%s' is not readable or has an invalid getter method: "
			+ "Does the return type of the getter match the parameter type of the setter?", propertyName));
	}
}
