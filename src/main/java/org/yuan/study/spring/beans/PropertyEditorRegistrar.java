package org.yuan.study.spring.beans;

public interface PropertyEditorRegistrar {

	/**
	 * Register custom 'java.beans.PropertyEditor.PropertyEditors' with 
	 * the given 'PropertyEditorRegistry'.
	 * @param registry
	 */
	void registerCustomEditors(PropertyEditorRegistry registry);
}
