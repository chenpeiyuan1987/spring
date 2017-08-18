package org.yuan.study.spring.beans.factory.config;

import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.factory.ListableBeanFactory;
import org.yuan.study.spring.beans.factory.NoSuchBeanDefinitionException;

public interface ConfigurableListableBeanFactory extends ListableBeanFactory,
	ConfigurableBeanFactory, AutowireCapableBeanFactory {
	
	/**
	 * Return the registered BeanDefinition for the given bean, 
	 * allowing access to its property values and constructor argument value.
	 * @param beanName
	 * @return
	 */
	BeanDefinition getBeanDefinition(String beanName) throws NoSuchBeanDefinitionException;
	
	/**
	 * Ignore the given dependency interface for autowiring.
	 * @param ifc
	 */
	void ignoreDependencyInterface(Class<?> ifc);
	
	/**
	 * Ignore the given dependency type for autowiring: for example, String.
	 * @param type
	 */
	void ignoreDependencyType(Class<?> type);
	
	/**
	 * Return that all non-lazy-init singletons are instantiated, also considering FactoryBeans.
	 */
	void preInstantiateSingletons() throws BeansException;
}
