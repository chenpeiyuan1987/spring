package org.yuan.study.spring.util;

public abstract class Assert {

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
}
