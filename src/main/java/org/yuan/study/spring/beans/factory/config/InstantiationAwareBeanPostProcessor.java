package org.yuan.study.spring.beans.factory.config;

import org.yuan.study.spring.beans.BeansException;

public interface InstantiationAwareBeanPostProcessor extends BeanPostProcessor {

	/**
	 * Apply this BeanPostProcessor before the target bean gets instantiated.
	 * @param beanClass
	 * @param beanName
	 * @throws BeansException
	 */
	Object postProcessBeforeInstantiation(Class<?> beanClass, String beanName) throws BeansException;
}
