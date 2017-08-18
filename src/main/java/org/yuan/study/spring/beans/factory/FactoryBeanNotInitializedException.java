package org.yuan.study.spring.beans.factory;

public class FactoryBeanNotInitializedException extends BeanCreationException {
	private static final long serialVersionUID = 1L;

	/**
	 * Create a new FactoryBeanNotInitializedException.
	 * @param beanName
	 * @param message
	 */
	public FactoryBeanNotInitializedException(String beanName, String message) {
		super(beanName, message);
	}

}
