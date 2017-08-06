package org.yuan.study.spring.beans;

public class NotWritablePropertyException extends BeansException {
	private static final long serialVersionUID = 1L;

	public NotWritablePropertyException(String message) {
		super(message);
		// TODO Auto-generated constructor stub
	}
	
	public NotWritablePropertyException(Class<?> beanClass, String propertyName, String message, Throwable ex) {
		super(message);
		// TODO Auto-generated constructor stub
	}

}
