package org.yuan.study.spring.beans;

public interface BeanWrapper extends PropertyAccessor, PropertyEditorRegistry {
	
	/**
	 * 
	 * @return
	 */
	Object getWrappedInstance();
}
