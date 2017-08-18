package org.yuan.study.spring.beans.factory.config;

public interface BeanPostProcessor {

	/**
	 * Apply this BeanPostProcessor to the given new bean instance after any bean 
	 * initialization callbacks. 
	 * @param bean
	 * @param name
	 * @return
	 */
	Object postProcessAfterInitialization(Object bean, String name);

	/**
	 * Apply this BeanPostProcessor to the given new bean instance before any bean 
	 * initialization callbacks. 
	 * @param bean
	 * @param name
	 * @return
	 */
	Object postProcessBeforeInitialization(Object bean, String name);
	
}
