package org.yuan.study.spring.beans.factory.support;

import java.lang.reflect.Method;

public abstract class MethodOverride {

	private final String methodName;
	
	private boolean overloaded = true;

	/**
	 * Construct a new override for the given method.
	 * @param methodName
	 */ 
	protected MethodOverride(String methodName) {
		this.methodName = methodName;
	}

	/**
	 * Return whether the overridden method has to be considered as overloaded
	 * (that is, whether arg type matching has to happen).
	 * @return
	 */
	public boolean isOverloaded() {
		return overloaded;
	}

	/**
	 * Set whether the overridden method has to be considered as overloaded
	 * (that is, whether arg type matching has to happen).
	 * @param overloaded
	 */
	public void setOverloaded(boolean overloaded) {
		this.overloaded = overloaded;
	}

	/**
	 * Return the name of the method to be overridden.
	 * @return
	 */
	public String getMethodName() {
		return methodName;
	}
	
	/**
	 * Subclasses must override this to indicate whether they match the given method.
	 * This allows for argument list checking as well as method name checking.
	 * @param method
	 * @return
	 */
	public abstract boolean matches(Method method);
}
