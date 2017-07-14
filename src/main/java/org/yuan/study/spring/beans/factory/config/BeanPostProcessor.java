package org.yuan.study.spring.beans.factory.config;

public interface BeanPostProcessor {

	/**
	 * 
	 * @param bean
	 * @param name
	 * @return
	 */
	Object postProcessAfterInitialization(Object bean, String name);

	/**
	 * 
	 * @param bean
	 * @param name
	 * @return
	 */
	Object postProcessBeforeInitialization(Object bean, String name);
	
	
}
