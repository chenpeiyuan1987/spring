package org.yuan.study.spring.beans.factory.config;

import org.yuan.study.spring.beans.factory.BeanFactory;

public interface AutowireCapableBeanFactory extends BeanFactory {
	
	/**  */
	int AUTOWIRE_AUTODETECT = 1;
	/**  */
	int AUTOWIRE_BY_NAME = 2;
	/**  */
	int AUTOWIRE_BY_TYPE = 3;
	/**  */
	int AUTOWIRE_CONSTRUCTOR = 4;
	
	/**
	 * 
	 * @param bean
	 * @param name
	 * @return
	 */
	Object applyBeanPostProcessorsAfterInitialization(Object bean, String name);
	
	/**
	 * 
	 * @param bean
	 * @param name
	 * @return
	 */
	Object applyBeanPostProcessorsBeforeInitialization(Object bean, String name);

	/**
	 * 
	 * @param bean
	 * @param name
	 */
	void applyBeanPropertyValues(Object bean, String name);
	
	/**
	 * 
	 * @param clazz
	 * @param mode
	 * @param check
	 * @return
	 */
	Object autowire(Class<?> clazz, int mode, boolean check);
	
	/**
	 * 
	 * @param bean
	 * @param mode
	 * @param check
	 */
	void autowireBeanProperties(Object bean, int mode, boolean check);
}
