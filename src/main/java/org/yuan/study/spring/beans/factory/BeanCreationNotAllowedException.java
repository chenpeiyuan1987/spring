package org.yuan.study.spring.beans.factory;

public class BeanCreationNotAllowedException extends BeanCreationException {
	private static final long serialVersionUID = 1L;

	/**
	 * Create a new BeanCreationNotAllowedException.
	 * @param beanName
	 * @param message
	 */
	public BeanCreationNotAllowedException(String beanName, String message) {
		super(beanName, message);
	}

}
