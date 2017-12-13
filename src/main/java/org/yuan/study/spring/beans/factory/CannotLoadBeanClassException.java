package org.yuan.study.spring.beans.factory;

import org.yuan.study.spring.beans.FatalBeanException;

public class CannotLoadBeanClassException extends FatalBeanException {
	private static final long serialVersionUID = 1L;

	private String resourceDescription;
	
	private String beanName;
	
	private String beanClassName;

	/**
	 * Create a new CannotLoadBeanClassException
	 * @param resourceDescription
	 * @param beanName
	 * @param beanClassName
	 * @param cause
	 */
	public CannotLoadBeanClassException(String resourceDescription, String beanName, String beanClassName, ClassNotFoundException cause) {
		super(String.format(
			"Cannot find class [%s] for bean with name '%s' defined in %s", 
				beanClassName, beanName, resourceDescription), cause);
		this.resourceDescription = resourceDescription;
		this.beanName = beanName;
		this.beanClassName = beanClassName;
	}
	
	/**
	 * Create a new CannotLoadBeanClassException.
	 * @param resourceDescription
	 * @param beanName
	 * @param beanClassName
	 * @param cause
	 */
	public CannotLoadBeanClassException(String resourceDescription, String beanName, String beanClassName, LinkageError cause) {
		super(String.format(
				"Error loading class [%s] for bean with name '%s' defined in %s", 
					beanClassName, beanName, resourceDescription), cause);
			this.resourceDescription = resourceDescription;
			this.beanName = beanName;
			this.beanClassName = beanClassName;
	}
	
	/**
	 * Return the description of the resource that the bean
	 * definition came from.
	 * @return
	 */
	public String getResourceDescription() {
		return resourceDescription;
	}

	/**
	 * Return the name of the bean requested.
	 * @return
	 */
	public String getBeanName() {
		return beanName;
	}

	/**
	 * Return the name of the class we were trying to load.
	 * @return
	 */
	public String getBeanClassName() {
		return beanClassName;
	}
	
}
