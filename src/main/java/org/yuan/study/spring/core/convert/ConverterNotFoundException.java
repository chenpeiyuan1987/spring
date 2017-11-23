package org.yuan.study.spring.core.convert;

public class ConverterNotFoundException extends ConversionException {
	private static final long serialVersionUID = 1L;

	private final TypeDescriptor sourceType;
	
	private final TypeDescriptor targetType;

	/**
	 * Creates a new conversion executor not found exception.
	 * @param sourceType
	 * @param targetType
	 */
	public ConverterNotFoundException(TypeDescriptor sourceType, TypeDescriptor targetType) {
		super(String.format(
			"No converter found capable of converting from '%s' to '%s'", 
				sourceType.getName(), targetType.getName()));
		this.sourceType = sourceType;
		this.targetType = targetType;
	}

	/**
	 * Returns the source type that was requested to convert from.
	 */
	public TypeDescriptor getSourceType() {
		return sourceType;
	}

	/**
	 * Returns the target type that was requested to convert to.
	 */
	public TypeDescriptor getTargetType() {
		return targetType;
	}
	
}
