package org.yuan.study.spring.beans.factory.support;

import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;

public class MethodOverrides {

	private final Set<MethodOverride> overrides = new HashSet<MethodOverride>();
	
	/**
	 * Create new MethodOverrides.
	 */
	public MethodOverrides() {
	}
	
	/**
	 * Deep copy constructor.
	 * @param other
	 */
	public MethodOverrides(MethodOverrides other) {
		addOverrides(other);
	}
	
	/**
	 * Copy all given method overrides into this object.
	 * @param other
	 */
	public void addOverrides(MethodOverrides other) {
		if (other != null) {
			this.overrides.addAll(other.getOverrides());
		}
	}
	
	/**
	 * Add the given method override.
	 * @param override
	 */
	public void addOverride(MethodOverride override) {
		this.overrides.add(override);
	}

	/**
	 * Return all method overrides contained by this object.
	 * @return
	 */
	public Set<MethodOverride> getOverrides() {
		return overrides;
	}
	
	/**
	 * Return whether the set of method overrides is empty.
	 * @return
	 */
	public boolean isEmpty() {
		return this.overrides.isEmpty();
	}
	
	/**
	 * Return the override for the given method, if any.
	 * @param method
	 * @return
	 */
	public MethodOverride getOverride(Method method) {
		for (MethodOverride methodOverride : overrides) {
			if (methodOverride.matches(method)) {
				return methodOverride;
			}
		}
		return null;
	}
	
}
