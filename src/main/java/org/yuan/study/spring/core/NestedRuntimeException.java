package org.yuan.study.spring.core;


public abstract class NestedRuntimeException extends RuntimeException {
	private static final long serialVersionUID = 1L;
	
	/**
	 * Create a new NestedRuntimeException.
	 * @param message
	 */
	public NestedRuntimeException(String message) {
		super(message);
	}
	
	/**
	 * Create a new NestedRuntimeException.
	 * @param message
	 * @param cause
	 */
	public NestedRuntimeException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * Return the detail message, 
	 * including the message from the nested exception if ther is one.
	 */
	public String getMessage() {
		String message = super.getMessage();
		Throwable cause = getCause();
		
		if (cause != null) {
			StringBuilder sb = new StringBuilder();
			if (message != null) {
				sb.append(message).append("; ");
			}
			sb.append("nested exception is ").append(cause);
			return sb.toString();
		} 
		else {
			return message;
		}
	}
	
	/**
	 * Check whether this exception contains an exception of the given class:
	 * either it is of the given class itself or it contains a nested cause of the given class.
	 * @param exClass
	 * @return
	 */
	public boolean contains(Class<?> exClass) {
		if (exClass == null) {
			return false;
		}
		if (exClass.isInstance(this)) {
			return true;
		}
		Throwable cause = getCause();
		if (cause == this) {
			return false;
		}
		if (cause instanceof NestedRuntimeException) {
			return ((NestedRuntimeException) cause).contains(exClass);
		}
		else {
			while (cause != null) {
				if (exClass.isInstance(cause)) {
					return true;
				}
				if (cause.getCause() == cause) {
					break;
				}
				cause = cause.getCause();
			}
			return false;
		}
	}

	/**
	 * Retrieve the innermost cause of this exception, if any.
	 * @return
	 */
	public Throwable getRootCause() {
		Throwable rootCause = null;
		Throwable cause = getCause();
		while (cause != null && cause != rootCause) {
			rootCause = cause;
			cause = cause.getCause();
		}
		return rootCause;
	}
	
	/**
	 * Retrieve the most specific cause of this exception, if any.
	 * @return
	 */
	public Throwable getMostSpecificCause() {
		Throwable rootCause = getRootCause();
		return (rootCause != null ? rootCause : this);
	}
}
