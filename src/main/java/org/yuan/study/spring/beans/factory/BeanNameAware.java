package org.yuan.study.spring.beans.factory;

public interface BeanNameAware {

	/**
	 * Set the name of the bean in the bean factory that created this bean.
	 * @param beanName
	 */
	void setBeanName(String beanName);
}
