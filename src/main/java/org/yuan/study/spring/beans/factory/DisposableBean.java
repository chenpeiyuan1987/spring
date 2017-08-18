package org.yuan.study.spring.beans.factory;

public interface DisposableBean {
	
	/**
	 * Invoked by a BeanFactory on destruction of a singleton.
	 */
	void destroy() throws Exception;
}
