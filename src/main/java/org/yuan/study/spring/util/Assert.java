package org.yuan.study.spring.util;

/**
 * Assertion utility class that assists in validating arguments.
 *
 */
public abstract class Assert {

	/**
	 * Assert a boolean expression, throwing IllegalArgumentException
	 * if the test result is false.
	 * @param expression
	 * @param message
	 */
	public static void isTrue(boolean expression, String message) {
		if (!expression) {
			throw new IllegalArgumentException(message);
		}
	}
	
	/**
	 * Assert a boolean expression, throwing IllegalArgumentException
	 * if the test result is false.
	 * @param expression
	 */
	public static void isTrue(boolean expression) {
		isTrue(expression, "[Assertion failed] - this expression must be true.");
	}
	
	/**
	 * Assert that an object is null.
	 * @param object
	 * @param message
	 */
	public static void isNull(Object object, String message) {
		if (object != null) {
			throw new IllegalArgumentException(message);
		}
	}
	
	/**
	 * Assert that an object is null.
	 * @param object
	 */
	public static void isNull(Object object) {
		isNull(object, "[Assertion failed] - the object argument must be null.");
	}
	
	/**
	 * Assert that a string is not empty;
	 * It must not be null and not empty.
	 * @param text
	 * @param message
	 */
	public static void hasLength(String text, String message) {
		if (!StringUtils.hasLength(text)) {
			throw new IllegalArgumentException(message);
		}
	}
	
	/**
	 * Assert that a string is not empty;
	 * It must not be null and not empty.
	 * @param text
	 */
	public static void hasLength(String text) {
		hasLength(text, "[Assertion failed] - this String argument must have length; it must not be null or empty. ");
	}
	
	/**
	 * Assert that a string has valid text content.
	 * It must not be null and must contain at least one non-whitespace character.
	 * @param text
	 * @param message
	 */
	public static void hasText(String text, String message) {
		if (!StringUtils.hasText(text)) {
			throw new IllegalArgumentException(message);
		}
	}
	
	/**
	 * Assert that a string has valid text content.
	 * It must not be null and must contain at least one non-whitespace character.
	 * @param text
	 */
	public static void hasText(String text) {
		hasText(text, "[Assertion failed] - this String argument must have text; it must not be null, empty or blank.");
	}
	
	/**
	 * Assert that an object is not null.
	 * @param object
	 * @param message
	 */
	public static void notNull(Object object, String message) {
		if(object == null) {
			throw new IllegalArgumentException(message);
		}
	}
	
	/**
	 * Assert that an object is not null.
	 * @param object
	 */
	public static void notNull(Object object) {
		notNull(object, "[Assertion failed] - this argument is required; it cannot be null.");
	}
	
	/**
	 * Assert that the provided object is an instance of the provided class.
	 * @param clazz
	 * @param object
	 */
	public static void isInstanceOf(Class<?> clazz, Object object) {
		isInstanceOf(clazz, object, "");
	}
	
	public static void isInstanceOf(Class<?> clazz, Object object, String message) {
		// TODO
	}
	/**
	 * Assert a boolean expression, throwing IllegalStateException
	 * if the test result is false.
	 * @param expression
	 * @param message
	 */
	public static void state(boolean expression, String message) {
		if (!expression) {
			throw new IllegalStateException(message);
		}
	}
	
	/**
	 * Assert a boolean expression, throwing IllegalStateException
	 * if the test result is false.
	 * @param expression
	 */
	public static void state(boolean expression) {
		state(expression, "[Assertion failed] - this state invariant must be true");
	}
}
