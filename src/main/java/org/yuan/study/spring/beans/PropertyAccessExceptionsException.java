package org.yuan.study.spring.beans;

import java.io.PrintStream;
import java.io.PrintWriter;

public class PropertyAccessExceptionsException extends BeansException {
	private static final long serialVersionUID = 1L;

	/***/
	private final BeanWrapper beanWrapper;
	
	/***/
	private final PropertyAccessException[] propertyAccessExceptions;
	
	/**
	 * 
	 * @param beanWrapper
	 * @param propertyAccessExceptions
	 */
	public PropertyAccessExceptionsException(BeanWrapper beanWrapper, PropertyAccessException[] propertyAccessExceptions) {
		super("");
		this.beanWrapper = beanWrapper;
		this.propertyAccessExceptions = propertyAccessExceptions;
	}

	/**
	 * 
	 * @return
	 */
	public BeanWrapper getBeanWrapper() {
		return beanWrapper;
	}

	/**
	 * 
	 * @return
	 */
	public PropertyAccessException[] getPropertyAccessExceptions() {
		return propertyAccessExceptions;
	}
	
	/**
	 * 
	 * @return
	 */
	public Object getBindObject() {
		return this.beanWrapper.getWrappedInstance();
	}
	
	/**
	 * 
	 * @return
	 */
	public int getExceptionCount() {
		return this.propertyAccessExceptions.length;
	}
	
	/**
	 * 
	 * @param propertyName
	 * @return
	 */
	public PropertyAccessException getPropertyAccessException(String propertyName) {
		// TODO
		return null;
	}

	@Override
	public String getMessage() {
		// TODO Auto-generated method stub
		return super.getMessage();
	}

	@Override
	public boolean contains(Class<?> exClass) {
		// TODO Auto-generated method stub
		return super.contains(exClass);
	}

	@Override
	public String toString() {
		// TODO Auto-generated method stub
		return super.toString();
	}

	@Override
	public void printStackTrace(PrintStream s) {
		// TODO Auto-generated method stub
		super.printStackTrace(s);
	}

	@Override
	public void printStackTrace(PrintWriter s) {
		// TODO Auto-generated method stub
		super.printStackTrace(s);
	}
	
	
}
