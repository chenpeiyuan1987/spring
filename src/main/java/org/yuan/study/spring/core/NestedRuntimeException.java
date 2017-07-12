package org.yuan.study.spring.core;

public abstract class NestedRuntimeException extends RuntimeException {
	private static final long serialVersionUID = 1L;

	public NestedRuntimeException(String message) {
		super(message);
	}
	
	public NestedRuntimeException(String message, Throwable cause) {
		super(message, cause);
	}

}
