package org.yuan.study.spring.beans;

import org.yuan.study.spring.core.convert.ConversionService;

public interface ConfigurablePropertyAccessor extends PropertyAccessor, PropertyEditorRegistry, TypeConverter {

	/**
	 * Specify a Spring 3.0 ConversionService to use for converting
	 * property values, as an alternative to JavaBeans PropertyEditors.
	 * @param conversionService
	 */
	void setConversionService(ConversionService conversionService);
	
	/**
	 * Return the associated ConversionService, if any.
	 * @return
	 */
	ConversionService getConversionService();
	
	/**
	 * Set whether to extract the old property value when applying a 
	 * property editor to a new value for a property.
	 * @param extractOldValueForEditor
	 */
	void setExtractOldValueForEditor(boolean extractOldValueForEditor);
	
	/**
	 * Return whether to extract the old property value when applying a 
	 * property editor to a new value for a property.
	 * @return
	 */
	boolean isExtractOldValueForEditor();
}
