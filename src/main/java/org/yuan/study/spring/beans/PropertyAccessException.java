package org.yuan.study.spring.beans;

import java.beans.PropertyChangeEvent;

import org.yuan.study.spring.core.ErrorCoded;

public abstract class PropertyAccessException extends BeansException implements ErrorCoded {
	private static final long serialVersionUID = 1L;
	
	private final PropertyChangeEvent propertyChangeEvent;

	/**
	 * Create a new PropertyAccessException
	 * @param propertyChangeEvent
	 * @param message
	 */
	public PropertyAccessException(PropertyChangeEvent propertyChangeEvent, String message) {
		super(message);
		this.propertyChangeEvent = propertyChangeEvent;
	}
	
	/**
	 * Create a new PropertyAccessException
	 * @param propertyChangeEvent
	 * @param message
	 */
	public PropertyAccessException(PropertyChangeEvent propertyChangeEvent, String message, Throwable ex) {
		super(message, ex);
		this.propertyChangeEvent = propertyChangeEvent;
	}

	/**
	 * Return the PropertyChangeEvent that resulted in the problem.
	 */
	public PropertyChangeEvent getPropertyChangeEvent() {
		return propertyChangeEvent;
	}

}
