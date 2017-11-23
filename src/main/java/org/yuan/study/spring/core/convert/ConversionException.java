package org.yuan.study.spring.core.convert;

import org.yuan.study.spring.core.NestedRuntimeException;

public abstract class ConversionException extends NestedRuntimeException {
	private static final long serialVersionUID = 1L;

	/**
	 * Construct a new conversion exception.
	 * @param message
	 * @param cause
	 */
	public ConversionException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * Construct a new conversion exception.
	 * @param message
	 */
	public ConversionException(String message) {
		super(message);
	}

}
