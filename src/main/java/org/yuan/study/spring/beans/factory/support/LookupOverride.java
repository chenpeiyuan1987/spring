package org.yuan.study.spring.beans.factory.support;

import java.lang.reflect.Method;

public class LookupOverride extends MethodOverride {

	private final String beanName;
	
	/**
	 * Construct a new LookupOverride.
	 * @param methodName
	 * @param beanName
	 */
	public LookupOverride(String methodName, String beanName) {
		super(methodName);
		this.beanName = beanName;
	}
	
	/**
	 * Return the name of the bean that should be returned by this method.
	 * @return
	 */
	public String getBeanName() {
		return beanName;
	}

	@Override
	public boolean matches(Method method) {
		return method.getName().equals(getMethodName());
	}

	@Override
	public String toString() {
		return String.format("LookupOverride for method '%s'; will return bean '%s'", getMethodName(), beanName);
	}

}
