package org.yuan.study.spring.util;

import java.lang.reflect.Method;

public abstract class ReflectionUtils {
	
	/**
	 * 
	 * @param targetClass
	 * @param methodName
	 * @param paramTypes
	 * @return
	 */
	public static Method findMethod(Class<?> targetClass, String methodName, Class<?>... paramTypes) {
		Assert.notNull(targetClass, "Class must not be null");
		Assert.notNull(methodName, "Method name must not be null");
		
		try {
			return targetClass.getMethod(methodName, paramTypes);
		} catch (NoSuchMethodException e) {
			return null;
		}
	}
}
