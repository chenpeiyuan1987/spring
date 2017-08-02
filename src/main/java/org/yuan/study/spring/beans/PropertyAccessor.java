package org.yuan.study.spring.beans;

import java.util.Map;

import org.springframework.beans.PropertyValue;

public interface PropertyAccessor {
	
	/**
	 * Path separator for nested properties.
	 */
	String NESTED_PROPERTY_SEPARATOR = ".";
	char NESTED_PROPERTY_SEPARATOR_CHAR = '.';
	
	/**
	 * Marker that indicates the start of a property key for an indexed or mapped property like "person.addresses[0]".
	 */
	String PROPERTY_KEY_PREFIX = "[";
	char PROPERTY_KEY_PREFIX_CHAR = '[';
	
	/**
	 * Marker that indicates the end of a property key for an indexed or mapped property like "person".
	 */
	String PROPERTY_KEY_SUFFIX = "]";
	char PROPERTY_KEY_SUFFIX_CHAR = ']';
	
	/**
	 * Get the current value of the specified property.
	 * @param propertyName
	 * @return
	 */
	Object getPropertyValue(String propertyName) throws BeansException;
	
	/**
	 * Set the specified value as current property value.
	 * @param pv
	 */
	void setPropertyValue(PropertyValue pv) throws BeansException;
	
	/**
	 * Set the specified value as current property value.
	 * @param propertyName
	 * @param value
	 */
	void setPropertyValue(String propertyName, Object value) throws BeansException;
	
	/**
	 * Perform a batch update from a Map.
	 * @param map
	 */
	void setPropertyValues(Map<String,Object> map) throws BeansException;
	
	/**
	 * The preferred way to perform a batch update.
	 * @param pvs
	 */
	void setPropertyValues(PropertyValues pvs) throws BeansException;
	
	/**
	 * Perform a batch update with full control over behavior.
	 * @param pvs
	 * @param ignoreUnknown
	 */
	void setPropertyValues(PropertyValues pvs, boolean ignoreUnknown) throws BeansException;
}
