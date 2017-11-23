package org.yuan.study.spring.beans;

import java.io.Serializable;

import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ObjectUtils;


public class PropertyValue extends BeanMetadataAttributeAccessor implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private final String name;
	
	private final Object value;

	/**
	 * Create a new PropertyValue instance.
	 * @param name
	 * @param value
	 */
	public PropertyValue(String name, Object value) {
		Assert.hasText(name, "Property name cannot be null");
		
		this.name = name;
		this.value = value;
	}

	/**
	 * Return the name of the property.
	 * @return
	 */
	public String getName() {
		return name;
	}

	/**
	 * Return the value of the property.
	 * @return
	 */
	public Object getValue() {
		return value;
	}

	@Override
	public int hashCode() {
		return this.name.hashCode() * 29 + (this.value == null ? 0 : this.value.hashCode());
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PropertyValue)) {
			return false;
		}
		PropertyValue otherPv = (PropertyValue) obj;
		return (this.name.equals(otherPv.name) && ObjectUtils.nullSafeEquals(this.value, otherPv.value));
	}

	@Override
	public String toString() {
		return String.format("PropertyValue: name='%s', value=[%s]", this.name, this.value);
	}
	
}
