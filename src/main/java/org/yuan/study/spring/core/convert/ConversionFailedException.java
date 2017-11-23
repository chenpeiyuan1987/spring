package org.yuan.study.spring.core.convert;

import org.yuan.study.spring.util.ObjectUtils;

public class ConversionFailedException extends ConversionException {
	private static final long serialVersionUID = 1L;

	private final TypeDescriptor sourceType;
	
	private final TypeDescriptor targetType;
	
	private final Object value;

	/**
	 * Create a new conversion exception.
	 * @param sourceType
	 * @param targetType
	 * @param value
	 * @param cause
	 */
	public ConversionFailedException(TypeDescriptor sourceType, TypeDescriptor targetType, Object value, Throwable cause) {
		super(String.format("Unable to convert value '%s' from type '%s' to type '%s'",
			ObjectUtils.nullSafeToString(value), sourceType.getName(), targetType.getName()), cause);
		this.sourceType = sourceType;
		this.targetType = targetType;
		this.value = value;
	}

	/**
	 * Return the source type we tried to convert the value from.
	 */
	public TypeDescriptor getSourceType() {
		return sourceType;
	}

	/**
	 * Return the source type we tried to convert the value to.
	 */
	public TypeDescriptor getTargetType() {
		return targetType;
	}

	/**
	 * Return the offending value.
	 */
	public Object getValue() {
		return value;
	}
	
}
