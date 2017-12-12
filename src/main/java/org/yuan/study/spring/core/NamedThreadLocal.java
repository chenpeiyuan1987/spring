package org.yuan.study.spring.core;

import org.yuan.study.spring.util.Assert;

public class NamedThreadLocal<T> extends ThreadLocal<T> {
	
	private final String name;

	
	/**
	 * Create a new NamedThreadLocal with the given name.
	 * @param name
	 */
	public NamedThreadLocal(String name) {
		Assert.hasText(name, "Name must not be empty");
		this.name = name;
	}

	@Override
	public String toString() {
		return name;
	}

}
