package org.yuan.study.spring.beans;

import java.beans.PropertyDescriptor;
import java.io.Serializable;

import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ObjectUtils;


public class PropertyValue extends BeanMetadataAttributeAccessor implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private final String name;
	
	private final Object value;
	
	private Object source;
	
	private boolean optional = false;
	
	private boolean converted = false;
	
	private Object convertedValue;
	
	/**
	 * Package-visible field that indicates whether conversion is necessary
	 */
	volatile Boolean conversionNecessary;
	
	/**
	 * Package-visible field for caching the resolved property path tokens
	 */
	volatile Object resolvedTokens;
	
	/**
	 * Package-visible field for caching the resolved PropertyDescriptor
	 */
	volatile PropertyDescriptor resolvedDescriptor;

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
	 * Copy constructor
	 * @param original
	 */
	public PropertyValue(PropertyValue original) {
		Assert.notNull(original, "Original must not be null");
		
		this.name = original.getName();
		this.value = original.getValue();
		this.source = original.getSource();
		this.optional = original.isOptional();
		this.converted = original.converted;
		this.convertedValue = original.convertedValue;
		this.conversionNecessary = original.conversionNecessary;
		this.resolvedTokens = original.resolvedTokens;
		this.resolvedDescriptor = original.resolvedDescriptor;
		copyAttributesFrom(original);
	}
	
	/**
	 * Constructor that exposes a new value for an original value holder.
	 * @param original
	 * @param newValue
	 */
	public PropertyValue(PropertyValue original, Object newValue) {
		Assert.notNull(original, "Original must not be null");
		
		this.name = original.getName();
		this.value = newValue;
		this.source = original;
		this.optional = original.isOptional();
		this.conversionNecessary = original.conversionNecessary;
		this.resolvedTokens = original.resolvedTokens;
		this.resolvedDescriptor = original.resolvedDescriptor;
		copyAttributesFrom(original);
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
	
	/**
	 * Return the original PropertyValue instance for this value holder.
	 * @return
	 */
	public PropertyValue getOriginalPropertyValue() {
		PropertyValue original = this;
		while (original.source instanceof PropertyValue && original.source != original) {
			original = (PropertyValue) original.source;
		}
		return original;
	}

	public boolean isOptional() {
		return optional;
	}

	public void setOptional(boolean optional) {
		this.optional = optional;
	}

	/**
	 * Return whether this holder contains a converted value already true,
	 * or whether the value still needs to be converted false.
	 * @return
	 */
	public synchronized boolean isConverted() {
		return converted;
	}

	/**
	 * return the converted value of the constructor argument,
	 * after processed type conversion.
	 * @return
	 */
	public synchronized Object getConvertedValue() {
		return convertedValue;
	}

	/**
	 * Set the converted value of the constructor argument,
	 * after processed type conversion.
	 */
	public synchronized void setConvertedValue(Object convertedValue) {
		this.converted = true;
		this.convertedValue = convertedValue;
	}

	@Override
	public int hashCode() {
		return name.hashCode() * 29 + ObjectUtils.nullSafeHashCode(value);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof PropertyValue)) {
			return false;
		}
		PropertyValue other = (PropertyValue) obj;
		return (name.equals(other.name) 
			&& ObjectUtils.nullSafeEquals(value, other.value)
			&& ObjectUtils.nullSafeEquals(source, other.source));
	}

	@Override
	public String toString() {
		return String.format("bean property '%s'", this.name);
	}
	
}
