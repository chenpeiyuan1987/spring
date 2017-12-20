package org.yuan.study.spring.beans.factory.config;

import org.yuan.study.spring.beans.BeanMetadataElement;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.ObjectUtils;

public class TypedStringValue implements BeanMetadataElement {

	private String value;
	
	private volatile Object targetType;
	
	private Object source;
	
	private String specifiedTypeName;
	
	private volatile boolean dynamic;
	
	/**
	 * Create a new TypedStringValue for the given String.
	 * @param value
	 * @param targetType
	 */
	public TypedStringValue(String value, Class<?> targetType) {
		setValue(value);
		setTargetType(targetType);
	}
	
	/**
	 * Create a new TypedStringValue for the given String.
	 * @param value
	 * @param targetType
	 */
	public TypedStringValue(String value, String targetTypeName) {
		setValue(value);
		setTargetTypeName(targetTypeName);
	}

	/**
	 * Create a new TypedStringValue for the given String.
	 * @param value
	 * @param targetType
	 */
	public TypedStringValue(String value) {
		setValue(value);
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
		if (!(targetType instanceof Class)) {
			throw new IllegalStateException("Typed String value does not carry a resolved target type");
		}
		return (Class<?>) targetType;
	}

	/**
	 * Set the type to covert to.
	 * @param targetType
	 */
	public void setTargetType(Class<?> targetType) {
		Assert.notNull(targetType, "targetType must not be null");
		this.targetType = targetType;
	}

	/**
	 * Specify the type to convert to.
	 * @param targetTypeName
	 */
	public void setTargetTypeName(String targetTypeName) {
		Assert.notNull(targetTypeName, "TargetTypeName must not be null");
		this.targetType = targetTypeName;
	}
	
	/**
	 * Return the type to convert to.
	 * @return
	 */
	public String getTargetTypeName() {
		if (targetType instanceof Class) {
			return ((Class<?>) targetType).getName();
		} 
		else {
			return (String) targetType;
		}
	}
	
	/**
	 * Return whether this typed String value carries a target type.
	 * @return
	 */
	public boolean hasTargetType() {
		return (targetType instanceof Class);
	}
	
	@Override
	public Object getSource() {
		return source;
	}

	/**
	 * Return the type name as actually specified for this particular value, if any.
	 * @return
	 */
	public String getSpecifiedTypeName() {
		return specifiedTypeName;
	}

	/**
	 * Set the type name as actually specified for this particular value, if any.
	 * @param specifiedTypeName
	 */
	public void setSpecifiedTypeName(String specifiedTypeName) {
		this.specifiedTypeName = specifiedTypeName;
	}

	/**
	 * Return whether this value has been marked as dynamic.
	 * @return
	 */
	public boolean isDynamic() {
		return dynamic;
	}

	/**
	 * Mark this value as dynamic, i.e. as containing an expression
	 * and hence not being subject to caching.
	 * @param dynamic
	 */
	public void setDynamic() {
		this.dynamic = true;
	}

	/**
	 * Set the configuration source Object for this metadata element.
	 * @param source
	 */
	public void setSource(Object source) {
		this.source = source;
	}
	
	/**
	 * Determine the type to convert to, resolving it from a specified class name
	 * if necessary.
	 * @param classLoader
	 * @return
	 * @throws ClassNotFoundException
	 */
	public Class<?> resolveTargetType(ClassLoader classLoader) throws ClassNotFoundException {
		if (targetType == null) {
			return null;
		}
		Class<?> resolvedClass = ClassUtils.forName(getTargetTypeName(), classLoader);
		targetType = resolvedClass;
		return resolvedClass;
	}

	@Override
	public int hashCode() {
		return ObjectUtils.nullSafeHashCode(value) * 29 
			+ ObjectUtils.nullSafeHashCode(targetType);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TypedStringValue)) {
			return false;
		}
		TypedStringValue other = (TypedStringValue) obj;
		return ObjectUtils.nullSafeEquals(value, other.value) 
			&& ObjectUtils.nullSafeEquals(targetType, other.targetType);
	}

	@Override
	public String toString() {
		return String.format("TypeStringValuee: value [%s], target type [%s]", value, targetType);
	}
	
}
