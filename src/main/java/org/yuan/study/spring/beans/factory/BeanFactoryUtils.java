package org.yuan.study.spring.beans.factory;

import org.yuan.study.spring.util.Assert;

public abstract class BeanFactoryUtils {
	
	/**
	 * Return the bean name, 
	 * stripping out the factory dereference prefix if necessary.
	 * @param name
	 * @return
	 */
	public static String transformedBeanName(String name) {
		Assert.notNull(name, "Name must not be null");
		
		if(name.startsWith(BeanFactory.FACTORY_BEAN_PREFIX)) {
			name = name.substring(BeanFactory.FACTORY_BEAN_PREFIX.length());
		}
		
		return name;
	}
	
	/**
	 * Return whether the given name is a factory dereference
	 * @param name
	 * @return
	 */
	public static boolean isFactoryDereference(String name) {
		return name.startsWith(BeanFactory.FACTORY_BEAN_PREFIX);
	}
}
