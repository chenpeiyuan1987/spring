package org.yuan.study.spring.beans.factory.config;

import org.springframework.util.Assert;

public class RuntimeBeanReference {

	private final String beanName;
	
	private final boolean toParent;
	
	/**
	 * Create a new RuntimeBeanReference to the given bean name,
	 * without explicitly marking it as reference to a bean in the parent factory.
	 * @param beanName
	 */
	public RuntimeBeanReference(String beanName) {
		this(beanName, false);
	}

	/**
	 * Create a new RuntimeBeanReference to the given bean name,
	 * with the option to mark it as reference to a bean in the parent factory.
	 * @param beanName
	 * @param toParent
	 */
	public RuntimeBeanReference(String beanName, boolean toParent) {
		Assert.hasText(beanName, "Bean name must not be empty");
		this.beanName = beanName;
		this.toParent = toParent;
	}

	/**
	 * Return the target bean name.
	 * @return
	 */
	public String getBeanName() {
		return beanName;
	}

	/**
	 * Return whether this is an explicit reference to a bean
	 * @return
	 */
	public boolean isToParent() {
		return toParent;
	}

	@Override
	public String toString() {
		return '<' + getBeanName() + '>';
	}
	
}
