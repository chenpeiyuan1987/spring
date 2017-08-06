package org.yuan.study.spring.beans;

public class InvalidPropertyException extends FatalBeanException {
	private static final long serialVersionUID = 1L;

	public InvalidPropertyException(Class<?> beanClass, String propertyName, String message, Throwable ex) {
		super(message);
		// TODO Auto-generated constructor stub
	}
	
	public InvalidPropertyException(Class<?> beanClass, String propertyName, String message) {
		super(message);
		// TODO Auto-generated constructor stub
	}
	
}
