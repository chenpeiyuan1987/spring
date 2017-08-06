package org.yuan.study.spring.beans.factory;

public class UnsatisfiedDependencyException extends BeanCreationException {
	private static final long serialVersionUID = 1L;

	public UnsatisfiedDependencyException(String beanName, String message) {
		super(beanName, message);
		// TODO Auto-generated constructor stub
	}

}