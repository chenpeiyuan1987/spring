package org.yuan.study.spring.core;

import java.io.PrintStream;
import java.io.PrintWriter;

public abstract class NestedRuntimeException extends RuntimeException {
	private static final long serialVersionUID = 1L;
	
	private Throwable cause;

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
		super(message);
		this.cause = cause;
	}

	/**
	 * Return the nested cause, or null if none.
	 */
	public Throwable getCause() {
		return (this.cause == this ? null : this.cause);
	}
	
	/**
	 * Return the detail message, 
	 * including the message from the nested exception if ther is one.
	 */
	public String getMessage() {
		String message = super.getMessage();
		Throwable cause = getCause();
		if (cause != null) {
			return message + "; nested exception is " + cause;
		}
		return message;
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

	@Override
	public void printStackTrace(PrintStream ps) {
		if (getCause() == null) {
			super.printStackTrace(ps);
		} 
		else {
			ps.println(this);
			getCause().printStackTrace(ps);
		}
	}

	@Override
	public void printStackTrace(PrintWriter pw) {
		if (getCause() == null) {
			super.printStackTrace(pw);
		} 
		else {
			pw.println(this);
			getCause().printStackTrace(pw);
		}
	}
	
}
