package org.yuan.study.spring.beans;

public class FatalBeanException extends BeansException {
	private static final long serialVersionUID = 1L;

	/**
	 * Create a new FatalBeanException with the specified message.
	 * @param message
	 */
	public FatalBeanException(String message) {
		super(message);
	}

	/**
	 * Create a new FatalBeanException with the specified message
	 * and root cause.
	 * @param message
	 * @param cause
	 */
	public FatalBeanException(String message, Throwable cause) {
		super(message, cause);
	}

}
