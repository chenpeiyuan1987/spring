package org.yuan.study.spring.beans;

public class FatalBeanException extends BeansException {
	private static final long serialVersionUID = 1L;

	public FatalBeanException(String message) {
		super(message);
	}

	public FatalBeanException(String message, Throwable cause) {
		super(message, cause);
	}

}
