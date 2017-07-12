package org.yuan.study.spring.beans.factory;

import java.util.Map;

import org.yuan.study.spring.beans.BeansException;

public interface ListableBeanFactory extends BeanFactory {
	/**
	 * Check if this bean factory contains a bean definition with the given name.
	 * @param beanName
	 * @return
	 */
	boolean containsBeanDefinition(String name);
	
	/**
	 * Return the number of beans defined in the factory.
	 * @return
	 */
	int getBeanDefinitionCount();
	
	/**
	 * Return the names of all beans defined in this factory.
	 * @return
	 */
	String[] getBeanDefinitionNames();
	
	/**
	 * 
	 * @param type
	 * @return
	 */
	String[] getBeanDefinitionNames(Class<?> type);
	
	/**
	 * Return the names of beans matching the given type
	 * @param Type
	 * @return
	 */
	String[] getBeanNamesForType(Class<?> type);
	
	/**
	 * Return the names of beans matching the given type
	 * @param type
	 * @param includePrototypes
	 * @param includeFactoryBeans
	 * @return
	 */
	String[] getBeanNamesForType(Class<?> type, boolean includePrototypes, boolean includeFactoryBeans);
	
	/**
	 * Return the bean instances that match the given object type
	 * @param type
	 * @return
	 */
	Map<String,Object> getBeansOfType(Class<?> type) throws BeansException;
	
	/**
	 * Return the bean instances that match the given object type
	 * @param type
	 * @param includePrototypes
	 * @param includeFactoryBeans
	 * @return
	 */
	Map<String,Object> getBeansOfType(Class<?> type, boolean includePrototypes, boolean includeFactoryBeans) throws BeansException;
}
