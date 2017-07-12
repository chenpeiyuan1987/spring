package org.yuan.study.spring.beans;

import org.yuan.study.spring.core.NestedRuntimeException;

public class BeansException extends NestedRuntimeException {
	private static final long serialVersionUID = 1L;

	public BeansException(String message) {
		super(message);
	}

	public BeansException(String message, Throwable cause) {
		super(message, cause);
	}

}
