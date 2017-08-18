package org.yuan.study.spring.beans.factory;

import org.yuan.study.spring.beans.FatalBeanException;
import org.yuan.study.spring.core.io.Resource;

public class BeanDefinitionStoreException extends FatalBeanException {
	private static final long serialVersionUID = 1L;
	
	private String resourceDescription;
	
	private String beanName;

	/**
	 * Create a new BeanDefinitionStoreException.
	 * @param message
	 */
	public BeanDefinitionStoreException(String message) {
		super(message);
	}
	
	/**
	 * Create a new BeanDefinitionStoreException.
	 * @param message
	 * @param ex
	 */
	public BeanDefinitionStoreException(String message, Throwable ex) {
		super(message, ex);
	}
	
	/**
	 * Create a new BeanDefinitionStoreException.
	 * @param resourceDescription
	 * @param beanName
	 * @param message
	 * @param ex
	 */
	public BeanDefinitionStoreException(String resourceDescription, String beanName, String message, Throwable ex) {
		super(String.format("Error registering bean with name '%s' defined in %s: %s", beanName, resourceDescription, message), ex);
		this.resourceDescription = resourceDescription;
		this.beanName = beanName;
	}
	
	/**
	 * Create a new BeanDefinitionStoreException.
	 * @param resourceDescription
	 * @param beanName
	 * @param message
	 */
	public BeanDefinitionStoreException(String resourceDescription, String beanName, String message) {
		this(resourceDescription, beanName, message, null);
	}
	
	/**
	 * Create a new BeanDefinitionStoreException.
	 * @param resource
	 * @param beanName
	 * @param message
	 */
	public BeanDefinitionStoreException(Resource resource, String beanName, String message) {
		this(resource.getDescription(), beanName, message, null);
	}
	
	/**
	 * Create a new BeanDefinitionStoreException.
	 * @param resource
	 * @param beanName
	 * @param message
	 * @param ex
	 */
	public BeanDefinitionStoreException(Resource resource, String beanName, String message, Throwable ex) {
		this(resource.getDescription(), beanName, message, ex);
	}

	/**
	 * Return the description of the resource that the bean definition came from, if any.
	 * @return
	 */
	public String getResourceDescription() {
		return resourceDescription;
	}

	/**
	 * Return the name of the bean requested, if any.
	 * @return
	 */
	public String getBeanName() {
		return beanName;
	}

}
