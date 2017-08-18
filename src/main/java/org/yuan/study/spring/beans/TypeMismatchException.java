package org.yuan.study.spring.beans;

import java.beans.PropertyChangeEvent;

import org.yuan.study.spring.util.ClassUtils;

public class TypeMismatchException extends PropertyAccessException {
	private static final long serialVersionUID = 1L;

	public static final String ERROR_CODE = "typeMismatch";
	
	private final Class<?> requiredType;
	
	/**
	 * Create a new TypeMismatchException.
	 * @param propertyChangeEvent
	 * @param requiredType
	 * @param cause
	 */
	public TypeMismatchException(PropertyChangeEvent propertyChangeEvent, Class<?> requiredType, Throwable cause) {
		super(propertyChangeEvent, 
			String.format("Failed to convert property value of type [%s]%s%s", 
			(propertyChangeEvent.getNewValue() != null ? ClassUtils.getQualifiedName(propertyChangeEvent.getNewValue().getClass()) : null),
			(requiredType != null ? String.format(" to requried type [%s]", ClassUtils.getQualifiedName(requiredType)) : ""),
			(propertyChangeEvent.getPropertyName() != null ? String.format(" for property '%s'", propertyChangeEvent.getPropertyName()): "")),
			cause);
		this.requiredType = requiredType;
	}
	
	/**
	 * Create a new TypeMismatchException.
	 * @param propertyChangeEvent
	 * @param requiredType
	 */
	public TypeMismatchException(PropertyChangeEvent propertyChangeEvent, Class<?> requiredType) {
		this(propertyChangeEvent, requiredType, null);
	}

	public String getErrorCode() {
		return ERROR_CODE;
	}

	/**
	 * Return teh required target type, if any.
	 * @return
	 */
	public Class<?> getRequiredType() {
		return requiredType;
	}
	
}
