package org.yuan.study.spring.beans.factory.support;

import org.yuan.study.spring.beans.factory.config.BeanDefinition;

public interface BeanDefinitionRegistry {

	/**
	 * Check if this registry contains a bean definition with the given name.
	 * @param beanName
	 * @return
	 */
	boolean containsBeanDefinition(String beanName);
	
	/**
	 * Return the aliases for the given bean name, if definied.
	 * @param beanName
	 * @return
	 */
	String[] getAliases(String beanName);
	
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
	 * Given a bean name, create an alias.
	 * @param beanName
	 * @param alias
	 */
	void registerAlias(String beanName, String alias);
	
	/**
	 * Register a new bean definition with this registry.
	 * @param beanName
	 * @param beanDefinition
	 */
	void registerBeanDefinition(String beanName, BeanDefinition beanDefinition);
}
