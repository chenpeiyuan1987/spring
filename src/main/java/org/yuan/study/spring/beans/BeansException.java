package org.yuan.study.spring.beans;

import org.yuan.study.spring.core.NestedRuntimeException;

public class BeansException extends NestedRuntimeException {
	private static final long serialVersionUID = 1L;

	/**
	 * Create a new BeansException with the specified message.
	 * @param message
	 */
	public BeansException(String message) {
		super(message);
	}

	/**
	 * Create a new BeansException with the specified message.
	 * @param message
	 * @param cause
	 */
	public BeansException(String message, Throwable cause) {
		super(message, cause);
	}

}
