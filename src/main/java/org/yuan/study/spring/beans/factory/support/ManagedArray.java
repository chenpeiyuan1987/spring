package org.yuan.study.spring.beans.factory.support;

import org.yuan.study.spring.util.Assert;

public class ManagedArray extends ManagedList<Object> {
	private static final long serialVersionUID = 1L;
	
	volatile Class<?> resolvedElementType;

	/**
	 * Create a new managed array placeholder.
	 * @param elementTypeName
	 * @param size
	 */
	public ManagedArray(String elementTypeName, int size) {
		super(size);
		Assert.notNull(elementTypeName, "elementTypeName must not be null");
		setElementTypeName(elementTypeName);
	}
	
}
