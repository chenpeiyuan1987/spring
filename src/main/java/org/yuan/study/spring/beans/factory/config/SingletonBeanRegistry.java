package org.yuan.study.spring.beans.factory.config;

public interface SingletonBeanRegistry {

	/**
	 * Register the given existing object as singleton in the bean registry,
	 * under the given bean name.
	 * @param beanName
	 * @param singletonObject
	 */
	void registerSingleton(String beanName, Object singletonObject);
	
	/**
	 * Return the singleton object registered under the given name.
	 * @param beanName
	 * @return
	 */
	Object getSingleton(String beanName);
	
	/**
	 * Check if this registry contains a singleton instance with the given name.
	 * @param beanName
	 * @return
	 */
	boolean containsSingleton(String beanName);
	
	/**
	 * Return the names of singleton beans registered in this registry.
	 * @return
	 */
	String[] getSingletonNames();
	
	/**
	 * Return the number of singleton beans registered in this registry.
	 * @return
	 */
	int getSingletonCount();
}
