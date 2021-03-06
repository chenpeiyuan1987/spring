package org.yuan.study.spring.beans.factory.support;

import org.yuan.study.spring.beans.FatalBeanException;

public class BeanDefinitionValidationException extends FatalBeanException {
	private static final long serialVersionUID = 1L;

	/**
	 * Create a new BeanDefinitionValidationException.
	 * @param message
	 */
	public BeanDefinitionValidationException(String message) {
		super(message);
	}

	/**
	 * Create a new BeanDefinitionValidationException.
	 * @param message
	 * @param cause
	 */
	public BeanDefinitionValidationException(String message, Throwable cause) {
		super(message, cause);
	}
	
}
