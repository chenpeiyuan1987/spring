package org.yuan.study.spring.beans;

import java.io.PrintStream;
import java.io.PrintWriter;

import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ObjectUtils;

public class PropertyBatchUpdateException extends BeansException {
	private static final long serialVersionUID = 1L;
	
	/** List of PropertyAccessException objects */
	private PropertyAccessException[] propertyAccessExceptions;

	/**
	 * Create a new PropertyBatchUpdateException.
	 * @param message
	 * @param propertyAccessExceptions
	 */
	public PropertyBatchUpdateException(PropertyAccessException[] propertyAccessExceptions) {
		super(null);
		Assert.notNull(propertyAccessExceptions, "At least 1 PropertyAccessException required");
		this.propertyAccessExceptions = propertyAccessExceptions;
	}

	@Override
	public String getMessage() {
		StringBuilder sb = new StringBuilder("Failed properties: ");
		for (int i = 0; i < propertyAccessExceptions.length; i++) {
			sb.append(propertyAccessExceptions[i].getMessage());
			if (i < propertyAccessExceptions.length - 1) {
				sb.append("; ");
			}
		}
		return sb.toString();
	}

	@Override
	public boolean contains(Class<?> exClass) {
		if (exClass == null) {
			return false;
		}
		if (exClass.isInstance(this)) {
			return true;
		}
		for (PropertyAccessException propertyAccessException : propertyAccessExceptions) {
			if (propertyAccessException.contains(exClass)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public void printStackTrace(PrintStream ps) {
		synchronized (ps) {
			ps.println(getClass().getName() + "; nested PropertyAccessException details (" + getExceptionCount() + ") are:");
			for (int i = 0; i < propertyAccessExceptions.length; i++) {
				ps.println("PropertyAccessException " + (i + 1) + ":");
				propertyAccessExceptions[i].printStackTrace(ps);
			}
		}
	}

	@Override
	public void printStackTrace(PrintWriter pw) {
		synchronized (pw) {
			pw.println(getClass().getName() + "; nested PropertyAccessException details (" + getExceptionCount() + ") are:");
			for (int i = 0; i < propertyAccessExceptions.length; i++) {
				pw.println("PropertyAccessException " + (i + 1) + ":");
				propertyAccessExceptions[i].printStackTrace(pw);
			}
		}
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(getClass().getName()).append("; nested PropertyAccessExceptions (");
		sb.append(getExceptionCount()).append(") are:");
		for (int i = 0; i < propertyAccessExceptions.length; i++) {
			sb.append("\n").append("PropertyAccessException ").append(i + 1).append(": ");
			sb.append(propertyAccessExceptions[i]);
		}
		return sb.toString();
	}

	/**
	 * Return an array of the propertyAccessExceptions stored in this object.
	 * @return
	 */
	public PropertyAccessException[] getPropertyAccessExceptions() {
		return propertyAccessExceptions;
	}
	
	/**
	 * Return the exception for this field, or null if there isn't any.
	 * @param propertyName
	 * @return
	 */
	public PropertyAccessException getPropertyAccessException(String propertyName) {
		for (PropertyAccessException propertyAccessException : propertyAccessExceptions) {
			if (ObjectUtils.nullSafeEquals(propertyName, propertyAccessException.getPropertyName())) {
				return propertyAccessException;
			}
		}
		return null;
	}
	
	/**
	 * If this returns 0, no errors were encountered during binding.
	 * @return
	 */
	public final int getExceptionCount() {
		return propertyAccessExceptions.length;
	}
}
