package org.yuan.study.spring.beans.factory;

import org.yuan.study.spring.beans.FatalBeanException;

public class BeanDefinitionStoreException extends FatalBeanException {
	private static final long serialVersionUID = 1L;

	public BeanDefinitionStoreException(String message) {
		super(message);
	}
	
	public BeanDefinitionStoreException(String resourceDescription, String beanName, String message, Throwable ex) {
		super(message);
	}
	
	public BeanDefinitionStoreException(String resourceDescription, String beanName, String message) {
		super(message);
	}

}
