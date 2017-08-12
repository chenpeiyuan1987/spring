package org.yuan.study.spring.beans.factory.support;

import org.yuan.study.spring.beans.factory.BeanDefinitionStoreException;
import org.yuan.study.spring.core.io.Resource;
import org.yuan.study.spring.core.io.ResourceLoader;

public interface BeanDefinitionReader {

	/**
	 * Return the class loader to use for bean classes.
	 * @return
	 */
	ClassLoader getBeanClassLoader();
	
	/**
	 * Return the bean factory to register the bean definitions with.
	 * @return
	 */
	BeanDefinitionRegistry getBeanFactory();
	
	/**
	 * Return the resource loader to use for resource locations.
	 * @return
	 */
	ResourceLoader getResourceLoader();
	
	/**
	 * Load bean definitions from the specified resource.
	 * @param resource
	 * @return
	 */
	int loadBeanDefinitions(Resource resource) throws BeanDefinitionStoreException;
	
	/**
	 * Load bean definitions from the specified resources.
	 * @param resources
	 * @return
	 */
	int loadBeanDefinitions(Resource[] resources) throws BeanDefinitionStoreException;
	
	/**
	 * Load bean definitions from the specified resource location.
	 * @param location
	 * @return
	 */
	int loadBeanDefinitions(String location) throws BeanDefinitionStoreException;
	
	/**
	 * Load bean definitions from the specified resource locations.
	 * @param locations
	 * @return
	 */
	int loadBeanDefinitions(String[] locations) throws BeanDefinitionStoreException;
	
}
