package org.yuan.study.spring.beans.factory.config;

import java.beans.PropertyEditor;

import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.HierarchicalBeanFactory;

public interface ConfigurableBeanFactory extends HierarchicalBeanFactory {

	/**
	 * 
	 * @param beanPostProcessor
	 */
	void addBeanPostProcessor(BeanPostProcessor beanPostProcessor);
	
	/**
	 * 
	 * @param beanName
	 * @return
	 */
	boolean containsSingleton(String beanName);
	
	/**
	 * 
	 * @param beanName
	 */
	void destroySingletons();
	
	/**
	 * 
	 * @return
	 */
	int getBeanPostProcessorCount();
	
	/**
	 * 
	 * @param beanName
	 * @param alias
	 */
	void registerAlias(String beanName, String alias);
	
	/**
	 * 
	 * @param requiredType
	 * @param propertyEditor
	 */
	void registerCustomEditor(Class<?> requiredType, PropertyEditor propertyEditor);
	
	/**
	 * 
	 * @param beanName
	 * @param singletonObject
	 */
	void registerSingleton(String beanName, Object singletonObject);
	
	/**
	 * 
	 * @param parentBeanFactory
	 */
	void setParentBeanFactory(BeanFactory parentBeanFactory);
}
