package org.yuan.study.spring.beans.factory.config;

import org.yuan.study.spring.beans.BeansException;

public interface DestructionAwareBeanPostProcessor extends BeanPostProcessor {

	/**
	 * Apply this BeanPostProcessor to the given bean instance before its destruction.
	 * Can invoke custom destruction callbacks.
	 * @param bean
	 * @param beanName
	 * @throws BeansException
	 */
	void postProcessBeforeDestruction(Object bean, String beanName) throws BeansException;
}
