package org.yuan.study.spring.util;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;

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
	 * Parse the given text into a number instance of the given target class,
	 * using the corresponding decode / valueOf methods.
	 * @param text
	 * @param targetClass
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static <T extends Number> T parseNumber(String text, Class<T> targetClass) {
		Assert.notNull(text, "Text must not be null");
		Assert.notNull(targetClass, "Target class must not be null");
		String trimmed = StringUtils.trimAllWhitespace(text);
		
		if (targetClass.equals(Byte.class)) {
			return (T) (isHexNumber(trimmed) ? Byte.decode(trimmed) : Byte.valueOf(trimmed));
		}
		if (targetClass.equals(Short.class)) {
			return (T) (isHexNumber(trimmed) ? Short.decode(trimmed) : Short.valueOf(trimmed));
		}
		if (targetClass.equals(Integer.class)) {
			return (T) (isHexNumber(trimmed) ? Short.decode(trimmed) : Short.valueOf(trimmed));
		}
		if (targetClass.equals(Long.class)) {
			return (T) (isHexNumber(trimmed) ? Long.decode(trimmed) : Long.valueOf(trimmed));
		}
		if (targetClass.equals(Float.class)) {
			return (T) Float.valueOf(trimmed);
		}
		if (targetClass.equals(Double.class)) {
			return (T) Double.valueOf(trimmed);
		}
		if (targetClass.equals(BigInteger.class)) {
			return (T) (isHexNumber(trimmed) ? decodeBigInteger(trimmed) : new BigInteger(trimmed));
		}
		if (targetClass.equals(BigDecimal.class) || targetClass.equals(Number.class)) {
			return (T) new BigDecimal(trimmed);
		}
		
		throw new IllegalArgumentException(String.format(
			"Cannot convert String [%s] to target class [%s]", text, targetClass.getName()));
	}
	
	/**
	 * Parse the given text into a number instance of the given target class,
	 * using the given NumberFormat. Trims the input String before attempting
	 * to parse the number.
	 * @param text
	 * @param targetClass
	 * @param numberFormat
	 * @return
	 */
	public static <T extends Number> T parseNumber(String text, Class<T> targetClass, NumberFormat numberFormat) {
		if (numberFormat != null) {
			Assert.notNull(text, "Text must not be null");
			Assert.notNull(targetClass, "Target class must not be null");
			
			DecimalFormat decimalFormat = null;
			boolean resetBigDecimal = false;
			if (numberFormat instanceof DecimalFormat) {
				decimalFormat = (DecimalFormat) numberFormat;
				if (BigDecimal.class.equals(targetClass) && !decimalFormat.isParseBigDecimal()) {
					decimalFormat.setParseBigDecimal(true);
					resetBigDecimal = true;
				}
			}
			try {
				Number number = numberFormat.parse(StringUtils.trimAllWhitespace(text));
				return convertNumberToTargetClass(number, targetClass);
			}
			catch (ParseException ex) {
				throw new IllegalArgumentException("Could not parse number: " + ex.getMessage());
			}
			finally {
				if (resetBigDecimal) {
					decimalFormat.setParseBigDecimal(false);
				}
			}
		}
		else {
			return parseNumber(text, targetClass);
		}
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
	 * Decode a BigInteger from a value.
	 * supports decimal, hex and octal notation.
	 * @param value
	 * @return
	 */
	private static BigInteger decodeBigInteger(String value) {
		int radix = 10;
		int index = 0;
		boolean negative = false;
		
		if (value.startsWith("-")) {
			negative = true;
			index++;
		}
		
		if (value.startsWith("0x", index) || value.startsWith("0X", index)) {
			index += 2;
			radix = 16;
		}
		if (value.startsWith("#", index)) {
			index++;
			radix = 16;
		}
		if (value.startsWith("0", index) && value.length() > 1 + index) {
			index++;
			radix = 8;
		}
		
		BigInteger result = new BigInteger(value.substring(index), radix);
		return (negative ? result.negate() : result);
	}
}
