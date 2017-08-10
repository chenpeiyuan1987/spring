package org.yuan.study.spring.beans.factory;

public class UnsatisfiedDependencyException extends BeanCreationException {
	private static final long serialVersionUID = 1L;

	public UnsatisfiedDependencyException(String beanName, String message) {
		super(beanName, message);
		// TODO Auto-generated constructor stub
	}
	
	public UnsatisfiedDependencyException(String resourceDescription, String beanName, String propertyName, String message) {
		super(beanName, message);
		// TODO Auto-generated constructor stub
	}
	
	public UnsatisfiedDependencyException(String resourceDescription, String beanName, int ctorArgIndex, Class<?> ctorArgType, String message) {
		super(beanName, message);
		// TODO Auto-generated constructor stub
	}

}
