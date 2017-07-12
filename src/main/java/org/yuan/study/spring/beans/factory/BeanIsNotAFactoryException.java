package org.yuan.study.spring.beans.factory;

public class BeanIsNotAFactoryException extends BeanNotOfRequiredTypeException {
	private static final long serialVersionUID = 1L;

	public BeanIsNotAFactoryException(String name, Class<?> actualType) {
		super(name, FactoryBean.class, actualType);
	}

}
