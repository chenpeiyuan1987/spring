package org.yuan.study.spring.beans;

import java.beans.PropertyEditor;

public interface PropertyEditorRegistry {

	/**
	 * Find a custom property editor for the given type and property.
	 * @param requiredType
	 * @param propertyPath
	 * @return
	 */
	PropertyEditor findCustomEditor(Class<?> requiredType, String propertyPath);
	
	/**
	 * Register the given custom property editor for all properties of the given type.
	 * @param requiredType
	 * @param propertyEditor
	 */
	void registerCustomEditor(Class<?> requiredType, PropertyEditor propertyEditor);
	
	/**
	 * Register the given custom property editor for the given type and property, or for all properties of the given type.
	 * @param requiredType
	 * @param propertyPath
	 * @param propertyEditor
	 */
	void registerCustomEditor(Class<?> requiredType, String propertyPath, PropertyEditor propertyEditor);
}
