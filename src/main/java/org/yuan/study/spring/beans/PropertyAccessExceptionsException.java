package org.yuan.study.spring.beans;

import java.io.PrintStream;
import java.io.PrintWriter;

public class PropertyAccessExceptionsException extends BeansException {
	private static final long serialVersionUID = 1L;

	/** BeanWrapper wrapping the target object for binding */
	private final BeanWrapper beanWrapper;
	
	/** List of  PropertyAccessException objects */
	private final PropertyAccessException[] propertyAccessExceptions;
	
	/**
	 * Create a new PropertyAccessExceptionsException.
	 * @param beanWrapper
	 * @param propertyAccessExceptions
	 */
	public PropertyAccessExceptionsException(BeanWrapper beanWrapper, PropertyAccessException[] propertyAccessExceptions) {
		super("");
		this.beanWrapper = beanWrapper;
		this.propertyAccessExceptions = propertyAccessExceptions;
	}

	/**
	 * Return the BeanWrapper that generated this exception.
	 * @return
	 */
	public BeanWrapper getBeanWrapper() {
		return beanWrapper;
	}

	/**
	 * Return an array of the propertyAccessExceptions stored in thsi object.
	 * @return
	 */
	public PropertyAccessException[] getPropertyAccessExceptions() {
		return propertyAccessExceptions;
	}
	
	/**
	 * Return the object we're binding to.
	 * @return
	 */
	public Object getBindObject() {
		return this.beanWrapper.getWrappedInstance();
	}
	
	/**
	 * If this returns 0, no errors were encountered during binding.
	 * @return
	 */
	public int getExceptionCount() {
		return this.propertyAccessExceptions.length;
	}
	
	/**
	 * Return the exception for this field, or null if there isn't one.
	 * @param propertyName
	 * @return
	 */
	public PropertyAccessException getPropertyAccessException(String propertyName) {
		for (PropertyAccessException propertyAccessException : propertyAccessExceptions) {
			if (propertyName.equals(propertyAccessException.getPropertyChangeEvent().getPropertyName())) {
				return propertyAccessException;
			}
		}
		return null;
	}

	@Override
	public String getMessage() {
		StringBuffer sb = new StringBuffer("Failed properties: ");
		for (int i = 0; i< propertyAccessExceptions.length; i++) {
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
	public String toString() {
		StringBuffer sb = new StringBuffer();
		sb.append(getClass().getName()).append("; nested PropertyAccessExceptions (");
		sb.append(getExceptionCount()).append(") are:");
		for (int i = 0; i < propertyAccessExceptions.length; i++) {
			sb.append("\n").append("PropertyAccessException ").append(i+1).append(": ");
			sb.append(propertyAccessExceptions[i]);
		}
		return sb.toString();
	}

	@Override
	public void printStackTrace(PrintStream ps) {
		ps.println(String.format("%s; nested PropertyAccessException details (%s) are:", getClass().getName(), getExceptionCount()));
		for (int i = 0; i < propertyAccessExceptions.length; i++) {
			ps.println(String.format("PropertyAccessException %s:", (i+1)));
			propertyAccessExceptions[i].printStackTrace(ps);
		}
	}

	@Override
	public void printStackTrace(PrintWriter pw) {
		pw.println(String.format("%s; nested PropertyAccessException details (%s) are:", getClass().getName(), getExceptionCount()));
		for (int i = 0; i < propertyAccessExceptions.length; i++) {
			pw.println(String.format("PropertyAccessException %s:", (i+1)));
			propertyAccessExceptions[i].printStackTrace(pw);
		}
	}
	
}
