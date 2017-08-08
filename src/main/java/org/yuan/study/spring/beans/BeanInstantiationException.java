package org.yuan.study.spring.beans;

public class BeanInstantiationException extends FatalBeanException {
	private static final long serialVersionUID = 1L;

	public BeanInstantiationException(Class<?> beanClass, String message) {
		super(message);
		// TODO Auto-generated constructor stub
	}
	
	public BeanInstantiationException(Class<?> beanClass, String message, Throwable cause) {
		super(message, cause);
		// TODO Auto-generated constructor stub
	}

}
