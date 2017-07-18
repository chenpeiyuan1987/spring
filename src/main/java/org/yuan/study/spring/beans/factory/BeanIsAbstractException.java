package org.yuan.study.spring.beans.factory;

public class BeanIsAbstractException extends BeanCreationException {
	private static final long serialVersionUID = 1L;

	public BeanIsAbstractException(String beanName) {
		super(beanName, "Bean definition is abstract");
	}

}
