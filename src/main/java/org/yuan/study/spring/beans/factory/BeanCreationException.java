package org.yuan.study.spring.beans.factory;

import org.yuan.study.spring.beans.FatalBeanException;

public class BeanCreationException extends FatalBeanException {
	private static final long serialVersionUID = 1L;
	
	/**  */
	private String beanName;
	

	public BeanCreationException(String beanName, String message) {
		super(message);
		this.beanName = beanName;
	}
	
	public BeanCreationException(String beanName, String message, Throwable cause) {
		super(message, cause);
		this.beanName = beanName;
	}
	
	public BeanCreationException(String resourceDescription, String beanName, String message) {
		super(message, null);
		this.beanName = beanName;
	}
	
	public BeanCreationException(String resourceDescription, String beanName, String message, Throwable cause) {
		super(message, cause);
		this.beanName = beanName;
	}
	
}
