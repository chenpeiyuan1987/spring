package org.yuan.study.spring.beans.factory;

import org.yuan.study.spring.beans.BeansException;

public interface BeanFactoryAware {

	/**
	 * Callback that supplies the owning factory to a bean instance.
	 * @param beanFactory
	 */
	void setBeanFactory(BeanFactory beanFactory) throws BeansException;
}
