package org.yuan.study.spring.core;

public abstract class NestedRuntimeException extends RuntimeException {
	private static final long serialVersionUID = 1L;
	
	private Throwable cause;

	public NestedRuntimeException(String message) {
		super(message);
	}
	
	public NestedRuntimeException(String message, Throwable cause) {
		super(message);
		this.cause = cause;
	}

	public Throwable getCause() {
		return (this.cause == this ? null : this.cause);
	}
	
	public String getMessage() {
		String message = super.getMessage();
		Throwable cause = getCause();
		if (cause != null) {
			return message + "; nested exception is " + cause;
		}
		return message;
	}
	
	public boolean contains(Class<?> exClass) {
		if (exClass == null) {
			return false;
		}
		
		Throwable ex = this;
		while (ex != null) {
			if (exClass.isInstance(ex)) {
				return true;
			}
			if (ex instanceof NestedRuntimeException) {
				ex = ((NestedRuntimeException) ex).getCause();
			}
			else {
				ex = null;
			}
		}
		
		return false;
	}
}
