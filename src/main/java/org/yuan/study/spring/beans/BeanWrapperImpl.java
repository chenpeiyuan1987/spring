package org.yuan.study.spring.beans;

public class BeanWrapperImpl extends PropertyEditorRegistrySupport implements BeanWrapper {

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
	
	
}
