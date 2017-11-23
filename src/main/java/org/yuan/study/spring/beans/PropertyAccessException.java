package org.yuan.study.spring.beans;

import java.beans.PropertyChangeEvent;

import org.yuan.study.spring.core.ErrorCoded;

public abstract class PropertyAccessException extends BeansException implements ErrorCoded {
	private static final long serialVersionUID = 1L;
	
	private transient PropertyChangeEvent propertyChangeEvent;
	
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
	 * Create a new PropertyAccessException without PropertyChangeEvent.
	 * @param propertyChangeEvent
	 * @param message
	 */
	public PropertyAccessException(String message, Throwable ex) {
		super(message, ex);
	}

	/**
	 * Return the PropertyChangeEvent that resulted in the problem.
	 */
	public PropertyChangeEvent getPropertyChangeEvent() {
		return propertyChangeEvent;
	}

	/**
	 * Return the name of the affected property, if available.
	 * @return
	 */
	public String getPropertyName() {
		return (propertyChangeEvent != null ? propertyChangeEvent.getPropertyName() : null);
	}
	
	/**
	 * Return the affected value that was about to be set, if any.
	 * @return
	 */
	public Object getValue() {
		return (propertyChangeEvent != null ? propertyChangeEvent.getNewValue() : null);
	}
}
