package org.yuan.study.spring.beans.factory;

public interface InitializingBean {

	/**
	 * Invoked by a BeanFactory after it has set all bean properties supplied.
	 */
	void afterPropertiesSet() throws Exception;
}
