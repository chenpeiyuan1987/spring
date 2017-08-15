package org.yuan.study.spring.beans.factory;

import org.yuan.study.spring.beans.FatalBeanException;
import org.yuan.study.spring.core.io.Resource;

public class BeanDefinitionStoreException extends FatalBeanException {
	private static final long serialVersionUID = 1L;

	public BeanDefinitionStoreException(String message) {
		super(message);
	}
	
	public BeanDefinitionStoreException(String message, Throwable ex) {
		super(message);
	}
	
	public BeanDefinitionStoreException(String resourceDescription, String beanName, String message, Throwable ex) {
		super(message);
	}
	
	public BeanDefinitionStoreException(String resourceDescription, String beanName, String message) {
		super(message);
	}
	
	public BeanDefinitionStoreException(Resource resource, String beanName, String message) {
		super(message);
	}
	
	public BeanDefinitionStoreException(Resource resource, String beanName, String message, Throwable ex) {
		super(message);
	}

}
