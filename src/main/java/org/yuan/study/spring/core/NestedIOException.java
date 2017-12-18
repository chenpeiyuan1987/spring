package org.yuan.study.spring.core;

import java.io.IOException;

public class NestedIOException extends IOException {
	private static final long serialVersionUID = 1L;

	public NestedIOException(String message, Throwable cause) {
		super(message);
		initCause(cause);
	}

	public NestedIOException(String message) {
		super(message);
	}

	@Override
	public String getMessage() {
		String message = super.getMessage();
		Throwable cause = getCause();
		
		if (cause != null) {
			StringBuilder sb = new StringBuilder();
			if (message != null) {
				sb.append(message).append("; ");
			}
			sb.append("nested exception is ").append(cause);
			return sb.toString();
		}
		else {
			return message;
		}
	}

}
