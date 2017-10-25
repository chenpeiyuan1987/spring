package org.yuan.study.spring.core.convert;

public interface ConversionService {

	/**
	 * Return true if objects of sourceType can be converted to targetType.
	 * @param sourceType
	 * @param targetType
	 * @return
	 */
	boolean canConvert(Class<?> sourceType, Class<?> targetType);
	
	/**
	 * Convert the source to targetType.
	 * @param source
	 * @param targetType
	 * @return
	 */
	<T> T convert(Object source, Class<T> targetType);
	
	/**
	 * Returns true if objects of sourceType can be converted to the targetType.
	 * @param sourceType
	 * @param targetType
	 * @return
	 */
	boolean canConvert(TypeDescriptor sourceType, TypeDescriptor targetType);
	
	/**
	 * Convert the source to targetType.
	 * @param source
	 * @param sourceType
	 * @param targetType
	 * @return
	 */
	Object convert(Object source, TypeDescriptor sourceType, TypeDescriptor targetType);
}
