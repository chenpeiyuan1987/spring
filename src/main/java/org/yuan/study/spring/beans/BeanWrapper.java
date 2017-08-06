package org.yuan.study.spring.beans;

import java.beans.PropertyDescriptor;

public interface BeanWrapper extends PropertyAccessor, PropertyEditorRegistry {
	
	/**
	 * Return the bean instance wrapped by this object, if any.
	 * @return
	 */
	Object getWrappedInstance();
	
	/**
	 * Change the wrapped JavaBean object.
	 */
	void setWrappedInstance(Object object);
	
	/**
	 * Obtain the PropertyDescriptors for the wrapped object.
	 * @return
	 */
	PropertyDescriptor[] getPropertyDescriptors();
	
	/**
	 * Obtain the property descriptor or a specific property of the wrapped object.
	 * @param propertyName
	 * @return
	 */
	PropertyDescriptor getPropertyDescriptor(String propertyName);
	
	/**
	 * Determine the property type for the specified property, 
	 * either checking the property descriptor or checking the value in case of an indexed or mapped element.
	 * @param propertyName
	 * @return
	 */
	Class<?> getPropertyType(String propertyName);
	
	/**
	 * Return the type of the wrapped JavaBean object.
	 * @return
	 */
	Class<?> getWrappedClass();
	
	/**
	 * Return whether to extract the old property value when applying a property editor to a new value for a property.
	 * @return
	 */
	boolean isExtractOldValueForEditor();
	
	/**
	 * Determine whether the specified property is readable.
	 * @return
	 */
	boolean isReadableProperty(String propertyName);
	
	/**
	 * Determine whether the specified property is writable.
	 * @return
	 */
	boolean isWritableProperty(String propertyName);
	
	/**
	 * Set whether to extract the old property value when applying a property editor to a new value for a property.
	 */
	void setExtractOldValueForEditor(boolean extractOldValueForEditor);
}
