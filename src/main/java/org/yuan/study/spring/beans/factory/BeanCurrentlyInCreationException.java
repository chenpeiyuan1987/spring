package org.yuan.study.spring.beans.factory;

public class BeanCurrentlyInCreationException extends BeanCreationException {
	private static final long serialVersionUID = 1L;

	/**
	 * Create a new BeanCurrentlyInCreationException.
	 * @param beanName
	 */
	public BeanCurrentlyInCreationException(String beanName) {
		super(beanName, 
			"Requested bean is currently in creation (circular reference when autowiring constructor?)");
	}
	
	/**
	 * Create a new BeanCurrentlyInCreationException.
	 * @param beanName
	 * @param message
	 */
	public BeanCurrentlyInCreationException(String beanName, String message) {
		super(beanName, message);
	}

}
