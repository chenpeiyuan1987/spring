package org.yuan.study.spring.util;

public abstract class Assert {

	/**
	 * Assert that a string has valid text content.
	 * It must not be null and must contain at least one non-whitespace character.
	 * @param str
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
	 * @param str
	 * @param message
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
	 * @param message
	 */
	public static void notNull(Object object) {
		notNull(object, "[Assertion failed] - this argument is required; it cannot be null.");
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
