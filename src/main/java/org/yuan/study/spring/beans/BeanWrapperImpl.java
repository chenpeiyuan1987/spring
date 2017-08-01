package org.yuan.study.spring.beans;

import java.beans.PropertyDescriptor;

public class BeanWrapperImpl extends PropertyEditorRegistrySupport implements BeanWrapper {

	/**
	 * 
	 */
	public BeanWrapperImpl() {
	}

	/**
	 * 
	 * @param bean
	 */
	public BeanWrapperImpl(Object bean) {
	}
	
	/**
	 * 
	 * @param newValue
	 * @param requiredType
	 * @return
	 * @throws TypeMismatchException
	 */
	public Object doTypeConversionIfNecessary(Object newValue, Class<?> requiredType) throws TypeMismatchException {
		return null;
	}

	@Override
	public Object getWrappedInstance() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public PropertyDescriptor[] getPropertyDescriptors() {
		// TODO Auto-generated method stub
		return null;
	}
	
}
