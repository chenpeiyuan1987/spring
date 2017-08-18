package org.yuan.study.spring.beans.factory.config;

import java.beans.PropertyEditor;

import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.HierarchicalBeanFactory;

public interface ConfigurableBeanFactory extends HierarchicalBeanFactory {

	/**
	 * Set the parent of this bean factory.
	 * @param beanPostProcessor
	 */
	void addBeanPostProcessor(BeanPostProcessor beanPostProcessor);
	
	/**
	 * Check if this bean factory contains a singleton instance with the given name.
	 * @param beanName
	 * @return
	 */
	boolean containsSingleton(String beanName);
	
	/**
	 * Destroy all cached singletons in this factory.
	 * @param beanName
	 */
	void destroySingletons();
	
	/**
	 * Return the current number of registered BeanPostProcessors.
	 * @return
	 */
	int getBeanPostProcessorCount();
	
	/**
	 * Given a bean name, create an alias. 
	 * We typically use this method to support names that are illegal within XML ids.
	 * @param beanName
	 * @param alias
	 */
	void registerAlias(String beanName, String alias) throws BeansException;
	
	/**
	 * Register the given custom property editor for all properties of the given type.
	 * @param requiredType
	 * @param propertyEditor
	 */
	void registerCustomEditor(Class<?> requiredType, PropertyEditor propertyEditor);
	
	/**
	 * Register the given existing object as singleton in the bean factory, 
	 * under the given bean name.
	 * @param beanName
	 * @param singletonObject
	 */
	void registerSingleton(String beanName, Object singletonObject) throws BeansException;
	
	/**
	 * Set the parent of this bean factory.
	 * @param parentBeanFactory
	 */
	void setParentBeanFactory(BeanFactory parentBeanFactory);
}
