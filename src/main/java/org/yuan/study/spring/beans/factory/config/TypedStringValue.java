package org.yuan.study.spring.beans.factory.config;

import org.yuan.study.spring.util.Assert;

public class TypedStringValue {

	private String value;
	
	private Class<?> targetType;
	
	/**
	 * Create a new TypedStringValue for the given String.
	 * @param value
	 * @param targetType
	 */
	public TypedStringValue(String value, Class<?> targetType) {
		super();
		this.value = value;
		this.targetType = targetType;
	}

	/**
	 * Return the String value.
	 * @return
	 */
	public String getValue() {
		return value;
	}

	/**
	 * Set the String value.
	 * @param value
	 */
	public void setValue(String value) {
		this.value = value;
	}

	/**
	 * Return the type to convert to
	 * @return
	 */
	public Class<?> getTargetType() {
		return targetType;
	}

	/**
	 * Set the type to covert to.
	 * @param targetType
	 */
	public void setTargetType(Class<?> targetType) {
		Assert.notNull(targetType, "targetType is required");
		this.targetType = targetType;
	}
	
}
