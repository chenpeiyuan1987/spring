package org.yuan.study.spring.beans.factory.support;

import java.lang.reflect.Method;

public interface MethodReplacer {

	/**
	 * Reimplement the given method
	 * @param obj
	 * @param method
	 * @param args
	 * @return
	 * @throws Throwable
	 */
	Object reimplement(Object obj, Method method, Object[] args) throws Throwable;
	
}
