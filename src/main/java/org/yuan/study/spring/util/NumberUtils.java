package org.yuan.study.spring.util;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.NumberFormat;

public abstract class NumberUtils {

	/**
	 * Convert the given number into an instance of the given target class.
	 * @param number
	 * @param targetClass
	 * @return
	 * @throws IllegalArgumentException
	 */
	@SuppressWarnings("unchecked")
	public static <T extends Number> T convertNumberToTargetClass(Number number, Class<?> targetClass) throws IllegalArgumentException {
		Assert.notNull(number, "Number must not be null");
		Assert.notNull(targetClass, "Target class must not be null");
		
		if (targetClass.isInstance(number)) {
			return (T) number;
		}
		if (targetClass.equals(Byte.class)) {
			long value = number.longValue();
			if (value < Byte.MIN_VALUE || value > Byte.MAX_VALUE) {
				raiseOverflowException(number, targetClass);
			}
			return (T) new Byte(number.byteValue());
		}
		if (targetClass.equals(Short.class)) {
			long value = number.longValue();
			if (value < Short.MIN_VALUE || value > Short.MAX_VALUE) {
				raiseOverflowException(number, targetClass);
			}
			return (T) new Short(number.byteValue());
		}
		if (targetClass.equals(Integer.class)) {
			long value = number.longValue();
			if (value < Integer.MIN_VALUE || value > Integer.MAX_VALUE) {
				raiseOverflowException(number, targetClass);
			}
			return (T) new Integer(number.byteValue());
		}
		if (targetClass.equals(BigInteger.class)) {
			if (number instanceof BigDecimal) {
				return (T) ((BigDecimal) number).toBigInteger();
			}
			else {
				return (T) BigInteger.valueOf(number.longValue());
			}
		}
		if (targetClass.equals(Float.class)) {
			return (T) new Float(number.floatValue());
		}
		if (targetClass.equals(Double.class)) {
			return (T) new Double(number.doubleValue());
		}
		if (targetClass.equals(BigDecimal.class)) {
			return (T) new BigDecimal(number.toString());
		}
		
		throw new IllegalArgumentException(String.format(
			"Could not convert number [%s] of type [%s] to unknown target class [%s]: overflow", 
			number, number.getClass().getName(), targetClass.getName()));
	}
	
	/**
	 * 
	 * @param text
	 * @param targetClass
	 * @return
	 */
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
	 * Raise an overflow exception for the given number and target class.
	 * @param number
	 * @param targetClass
	 */
	private static void raiseOverflowException(Number number, Class<?> targetClass) throws IllegalArgumentException {
		throw new IllegalArgumentException(String.format(
			"Could not convert number [%s] of type [%s] to target class [%s]: overflow", 
				number, number.getClass().getName(), targetClass.getName()));
	}
	
	/**
	 * Determine whether the given value String indicates a hex number, 
	 * i.e. needs to be passed into Integer.decode instead of Integer.valueOf.
	 * @param value
	 * @return
	 */
	private static boolean isHexNumber(String value) {
		int index = (value.startsWith("-") ? 1 : 0);
		return (value.startsWith("0x", index) || value.startsWith("0X", index) || value.startsWith("#", index));
	}
	
	/**
	 * 
	 * @param value
	 * @return
	 */
	private static BigInteger decodeBigInteger(String value) {
		int radix = 10;
		int index = 0;
		boolean negative = false;
	}
}
