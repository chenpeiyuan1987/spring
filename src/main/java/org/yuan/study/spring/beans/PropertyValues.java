package org.yuan.study.spring.beans;

public interface PropertyValues {

	/**
	 * Is there a property value for this property?
	 * @param name
	 * @return
	 */
	boolean contains(String propertyName);
	
	/**
	 * Return the changes since the previous PropertyValues
	 * @param old
	 * @return
	 */
	PropertyValues changesSince(PropertyValues old);
	
	/**
	 * Return the property value with the given name, if any.
	 * @param propertyName
	 * @return
	 */
	PropertyValue getPropertyValue(String propertyName);
	
	/**
	 * Return an array of the PropertyValue objects held in this object.
	 * @return
	 */
	PropertyValue[] getPropertyValues();
	
	/**
	 * Does this holder not contain any PropertyValue objects at all?
	 * @return
	 */
	boolean isEmpty();
}
