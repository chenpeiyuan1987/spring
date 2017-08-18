package org.yuan.study.spring.core;

public interface ControlFlow {

	/**
	 * Detect whether we're under the given class, according to the current stack trace.
	 * @param clazz
	 * @return
	 */
	boolean under(Class<?> clazz);
	
	/**
	 * Detect whether we're under the given class and method, according to the current stack trace.
	 * @param clazz
	 * @param methodName
	 * @return
	 */
	boolean under(Class<?> clazz, String methodName);
	
	/**
	 * Detect whether the current stack trace contains the given token.
	 * @param token
	 * @return
	 */
	boolean underToken(String token);
}
