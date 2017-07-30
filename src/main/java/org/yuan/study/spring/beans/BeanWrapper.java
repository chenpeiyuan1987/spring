package org.yuan.study.spring.beans;

import java.beans.PropertyDescriptor;

public interface BeanWrapper extends PropertyAccessor, PropertyEditorRegistry {
	
	/**
	 * 
	 * @return
	 */
	Object getWrappedInstance();
	
	/**
	 * 
	 * @return
	 */
	PropertyDescriptor[] getPropertyDescriptors();
}
