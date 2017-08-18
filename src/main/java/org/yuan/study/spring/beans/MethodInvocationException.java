package org.yuan.study.spring.beans;

import java.beans.PropertyChangeEvent;

public class MethodInvocationException extends PropertyAccessException {
	private static final long serialVersionUID = 1L;
	
	public static final String ERROR_CODE = "methodInvocation";
	
	/**
	 * Create a new MethodInvocationException.
	 * @param propertyChangeEvent
	 * @param ex
	 */
	public MethodInvocationException(PropertyChangeEvent propertyChangeEvent, Throwable ex) {
		super(propertyChangeEvent, String.format("Property '%s' threw exception", propertyChangeEvent.getPropertyName()), ex);
	}

	@Override
	public String getErrorCode() {
		return ERROR_CODE;
	}
	
}
