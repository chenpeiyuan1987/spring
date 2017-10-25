package org.yuan.study.spring.beans;

import org.yuan.study.spring.core.MethodParameter;

public interface TypeConverter {

	/**
	 * Convert the value to the required type (if necessary from a String).
	 * @param value
	 * @param requiredType
	 * @return
	 * @throws TypeMismatchException
	 */
	<T> T convertIfNecessary(Object value, Class<T> requiredType) throws TypeMismatchException;
	
	/**
	 * Convert the value to the required type (if necessary from a String).
	 * @param value
	 * @param requiredType
	 * @param methodParam
	 * @return
	 * @throws TypeMismatchException
	 */
	<T> T convertIfNecessary(Object value, Class<T> requiredType, MethodParameter methodParam) throws TypeMismatchException;
}
