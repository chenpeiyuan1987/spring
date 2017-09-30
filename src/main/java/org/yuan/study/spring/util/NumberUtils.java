package org.yuan.study.spring.util;

import java.text.NumberFormat;

public abstract class NumberUtils {

	public static <T extends Number> T parseNumber(String text, Class<T> targetClass) {
		Assert.notNull(text, "Text must not be null");
		Assert.notNull(targetClass, "Target class must not be null");
		
		return null;
	}
	
	/**
	 * 
	 * @param text
	 * @param targetClass
	 * @param numberFormat
	 * @return
	 */
	public static <T extends Number> T parseNumber(String text, Class<?> targetClass, NumberFormat numberFormat) {
		
		return null;
	}
	
	/**
	 * 
	 * @return
	 * @throws IllegalArgumentException
	 */
	public static Number convertNumberToTargetClass(Number number, Class<?> targetClass) throws IllegalArgumentException {
		// TODO
		return null;
	}
}
