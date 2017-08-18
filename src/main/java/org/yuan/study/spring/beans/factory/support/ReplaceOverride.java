package org.yuan.study.spring.beans.factory.support;

import java.lang.reflect.Method;
import java.util.LinkedList;
import java.util.List;

public class ReplaceOverride extends MethodOverride {

	private final String methodReplacerBeanName;
	
	private List<String> typeIdentifiers = new LinkedList<String>();
	
	/**
	 * Creaet a new ReplaceOverride
	 * @param methodName
	 * @param methodReplacerBeanName
	 */
	public ReplaceOverride(String methodName, String methodReplacerBeanName) {
		super(methodName);
		this.methodReplacerBeanName = methodReplacerBeanName;
	}
	
	/**
	 * Add a fragment of a class string. 
	 * like "Exception" or "java.lang.Exc", to identify a parameter type
	 * @param s
	 */
	public void addTypeIdentifier(String s) {
		typeIdentifiers.add(s);
	}

	/**
	 * return the name of the bean implementing MethodReplacer.
	 * @return
	 */
	public String getMethodReplacerBeanName() {
		return methodReplacerBeanName;
	}

	@Override
	public boolean matches(Method method) {
		if (!method.getName().endsWith(getMethodName())) {
			return false;
		}
		if (!isOverloaded()) {
			return true;
		}
		if (this.typeIdentifiers.size() != method.getParameterTypes().length) {
			return false;
		}
		for (int i = 0; i < typeIdentifiers.size(); i++) {
			String identifier = typeIdentifiers.get(i);
			if (method.getParameterTypes()[i].getName().indexOf(identifier) == -1) {
				return false;
			}
		}
		return true;
	}

	@Override
	public String toString() {
		return String.format("Replace override for method '%s'; will call bean '%s'", getMethodName(), this.methodReplacerBeanName);
	}

}
