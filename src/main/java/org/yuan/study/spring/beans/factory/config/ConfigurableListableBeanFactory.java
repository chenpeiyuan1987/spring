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
	
	/**
	 * Return whether this factory's bean definitions are frozen,
	 * i.e. are not supposed to be modified or post-processed any further.
	 * @return
	 */
	boolean isConfigurationFrozen();
	
	/**
	 * Freeze all bean definitions, signalling that the registered bean definitions
	 * will not be modified or post-processed any further.
	 */
	void freezeConfiguration();
	
	/**
	 * Determine whether the specified bean qualifies as an autowired value.
	 * @param beanName
	 * @param descriptor
	 * @return
	 * @throws NoSuchBeanDefinitionException
	 */
	boolean isAutowireCandidate(String beanName, DependencyDescriptor descriptor) throws NoSuchBeanDefinitionException;
	
	/**
	 * Register a special dependency type with corresponding autowired value.
	 * @param dependencyType
	 * @param autowiredValue
	 */
	void registerResolvableDependency(Class<?> dependencyType, Object autowiredValue);
}
