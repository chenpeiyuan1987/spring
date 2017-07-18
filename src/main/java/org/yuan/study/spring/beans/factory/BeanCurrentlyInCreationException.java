package org.yuan.study.spring.beans.factory;

public class BeanCurrentlyInCreationException extends BeanCreationException {
	private static final long serialVersionUID = 1L;

	public BeanCurrentlyInCreationException(String beanName) {
		super(beanName, null);
		// TODO Auto-generated constructor stub
	}

}
