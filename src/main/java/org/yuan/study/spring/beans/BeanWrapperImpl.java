package org.yuan.study.spring.beans;

public class BeanWrapperImpl extends PropertyEditorRegistrySupport implements BeanWrapper {

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
}
