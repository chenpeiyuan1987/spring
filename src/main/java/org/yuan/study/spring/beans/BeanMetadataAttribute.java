package org.yuan.study.spring.beans;

import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ObjectUtils;

public class BeanMetadataAttribute implements BeanMetadataElement {
	
	private final String name;
	
	private final Object value;
	
	private Object source;

	/**
	 * Create a new AttributeValue instance.
	 * @param name
	 * @param value
	 */
	public BeanMetadataAttribute(String name, Object value) {
		Assert.notNull(name, "Name must not be null");
		
		this.name = name;
		this.value = value;
	}

	/**
	 * Return the name of the attribute.
	 * @return
	 */
	public String getName() {
		return name;
	}

	/**
	 * Return the value of the attribute.
	 * @return
	 */
	public Object getValue() {
		return value;
	}

	/**
	 * Set the configuration source Object for this metadata element.
	 * @param source
	 */
	public void setSource(Object source) {
		this.source = source;
	}

	@Override
	public Object getSource() {
		return source;
	}

	@Override
	public int hashCode() {
		return name.hashCode() * 29 + ObjectUtils.nullSafeHashCode(value);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!(obj instanceof BeanMetadataAttribute))
			return false;
		BeanMetadataAttribute other = (BeanMetadataAttribute) obj;
		return (name.equals(other.name) && 
			ObjectUtils.nullSafeEquals(value, other.value) &&
			ObjectUtils.nullSafeEquals(source, other.source));
	}

	@Override
	public String toString() {
		return String.format("metadata attribute '%s'", name);
	}

	
}
