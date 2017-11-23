package org.yuan.study.spring.beans;

import java.beans.PropertyChangeEvent;

public class ConversionNotSupportedException extends TypeMismatchException {
	private static final long serialVersionUID = 1L;

	/**
	 * Create a new ConversionNotSupportedException.
	 * @param propertyChangeEvent
	 * @param requiredType
	 * @param cause
	 */
	public ConversionNotSupportedException(PropertyChangeEvent propertyChangeEvent, Class<?> requiredType, Throwable cause) {
		super(propertyChangeEvent, requiredType, cause);
	}

	/**
	 * Create a new ConversionNotSupportedException.
	 * @param propertyChangeEvent
	 * @param requiredType
	 */
	public ConversionNotSupportedException(Object value, Class<?> requiredType, Throwable cause) {
		super(value, requiredType, cause);
	}

}
