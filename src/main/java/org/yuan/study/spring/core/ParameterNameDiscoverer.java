package org.yuan.study.spring.core;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

public interface ParameterNameDiscoverer {

	/**
	 * Return parameter names for this method,
	 * or null if they cannot be determined.
	 * @param method
	 * @return
	 */
	String[] getParameterNames(Method method);
	
	/**
	 * Return parameter names for this constructor,
	 * or null if they cannot be determined.
	 * @param ctor
	 * @return
	 */
	String[] getParameterNames(Constructor ctor);
}
