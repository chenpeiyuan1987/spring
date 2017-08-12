package org.yuan.study.spring.beans.factory.support;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.yuan.study.spring.core.io.Resource;
import org.yuan.study.spring.core.io.ResourceLoader;


public abstract class AbstractBeanDefinitionReader implements BeanDefinitionReader {

	protected Log logger = LogFactory.getLog(getClass());

	private BeanDefinitionRegistry beanDefinitionRegistry;
	
	
	public AbstractBeanDefinitionReader(BeanDefinitionRegistry beanDefinitionRegistry) {
		this.beanDefinitionRegistry = beanDefinitionRegistry;
	}
	
	
	//------------------------------------------------------------
	// Implementation of methods
	//------------------------------------------------------------

	/**
	 * Set the ClassLoader to use for bean classes.
	 * @param beanClassLoader
	 */
	public void setBeanClassLoader(ClassLoader beanClassLoader) {
		
	}
	
	/**
	 * Set the ResourceLoader to use for resource locations.
	 * @param resourceLoader
	 */
	public void setResourceLoader(ResourceLoader resourceLoader) {
		
	}
	
	
	//------------------------------------------------------------
	// Implementation of BeanDefinitionReader interface
	//------------------------------------------------------------
	
	@Override
	public ClassLoader getBeanClassLoader() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public BeanDefinitionRegistry getBeanFactory() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ResourceLoader getResourceLoader() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int loadBeanDefinitions(Resource[] resources) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int loadBeanDefinitions(String location) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int loadBeanDefinitions(String[] locations) {
		// TODO Auto-generated method stub
		return 0;
	}
}
