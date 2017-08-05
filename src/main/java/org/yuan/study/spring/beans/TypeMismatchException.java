package org.yuan.study.spring.beans;

import java.beans.PropertyChangeEvent;

public class TypeMismatchException extends PropertyAccessException {
	private static final long serialVersionUID = 1L;

	public TypeMismatchException(String message, Throwable cause) {
		super(message);
		// TODO Auto-generated constructor stub
	}

	public TypeMismatchException(String message) {
		super(message);
		// TODO Auto-generated constructor stub
	}
	
	public TypeMismatchException(PropertyChangeEvent event, Class<?> clazz, Throwable cause) {
		super("");
		// TODO Auto-generated constructor stub
	}
	
	public TypeMismatchException(PropertyChangeEvent event, Class<?> clazz) {
		super("");
		// TODO Auto-generated constructor stub
	}
	
}
