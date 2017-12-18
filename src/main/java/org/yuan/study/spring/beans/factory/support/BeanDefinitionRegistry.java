package org.yuan.study.spring.beans.factory.support;

import org.yuan.study.spring.beans.factory.NoSuchBeanDefinitionException;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.core.AliasRegistry;

public interface BeanDefinitionRegistry extends AliasRegistry {

	/**
	 * Check if this registry contains a bean definition with the given name.
	 * @param beanName
	 * @return
	 */
	boolean containsBeanDefinition(String beanName);
	
	/**
	 * Return the BeanDefinition for the give bean name.
	 * @param beanName
	 * @return
	 */
	BeanDefinition getBeanDefinition(String beanName);
	
	/**
	 * Return the number of beans defined in this registry.
	 * @return
	 */
	int getBeanDefinitionCount();
	
	/**
	 * Return the names of all beans defined in this registry.
	 * @return
	 */
	String[] getBeanDefinitionNames();
	
	/**
	 * Register a new bean definition with this registry.
	 * @param beanName
	 * @param beanDefinition
	 */
	void registerBeanDefinition(String beanName, BeanDefinition beanDefinition);
	
	/**
	 * Remove the BeanDefinition for the given name.
	 * @param beanName
	 * @throws NoSuchBeanDefinitionException
	 */
	void removeBeanDefinition(String beanName) throws NoSuchBeanDefinitionException;
	
	/**
	 * Determine whether the given bean name is already in use within this registry,
	 * i.e. whether there is a local bean or alias registered under this name.
	 * @param beanName
	 * @return
	 */
	boolean isBeanNameInUse(String beanName);
}
