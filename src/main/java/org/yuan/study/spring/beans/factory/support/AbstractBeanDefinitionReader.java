package org.yuan.study.spring.beans.factory.support;

import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.yuan.study.spring.beans.factory.BeanDefinitionStoreException;
import org.yuan.study.spring.core.io.Resource;
import org.yuan.study.spring.core.io.ResourceLoader;
import org.yuan.study.spring.core.io.support.PathMatchingResourcePatternResolver;
import org.yuan.study.spring.core.io.support.ResourcePatternResolver;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ClassUtils;


public abstract class AbstractBeanDefinitionReader implements BeanDefinitionReader {

	protected Log logger = LogFactory.getLog(getClass());

	private BeanDefinitionRegistry beanDefinitionRegistry;
	
	private ResourceLoader resourceLoader;
	
	private ClassLoader beanClassLoader = ClassUtils.getDefaultClassLoader();
	
	
	/**
	 * Create a new AbstractBeanDefinitionReader for the given bean factory.
	 * @param beanDefinitionRegistry
	 */
	public AbstractBeanDefinitionReader(BeanDefinitionRegistry beanDefinitionRegistry) {
		Assert.notNull(beanDefinitionRegistry, "Bean factory must not be null");
		this.beanDefinitionRegistry = beanDefinitionRegistry;
		
		if (this.beanDefinitionRegistry instanceof ResourceLoader) {
			this.resourceLoader = (ResourceLoader) this.beanDefinitionRegistry;
		} 
		else {
			this.resourceLoader = new PathMatchingResourcePatternResolver();
		}
	}
	
	
	//------------------------------------------------------------
	// Implementation of methods
	//------------------------------------------------------------

	/**
	 * Set the ClassLoader to use for bean classes.
	 * @param beanClassLoader
	 */
	public void setBeanClassLoader(ClassLoader beanClassLoader) {
		this.beanClassLoader = beanClassLoader;
	}
	
	/**
	 * Set the ResourceLoader to use for resource locations.
	 * @param resourceLoader
	 */
	public void setResourceLoader(ResourceLoader resourceLoader) {
		this.resourceLoader = resourceLoader;
	}
	
	
	//------------------------------------------------------------
	// Implementation of BeanDefinitionReader interface
	//------------------------------------------------------------
	
	@Override
	public ClassLoader getBeanClassLoader() {
		return beanClassLoader;
	}

	@Override
	public BeanDefinitionRegistry getBeanFactory() {
		return beanDefinitionRegistry;
	}

	@Override
	public ResourceLoader getResourceLoader() {
		return resourceLoader;
	}

	@Override
	public int loadBeanDefinitions(Resource[] resources) throws BeanDefinitionStoreException {
		Assert.notNull(resources, "Resource array must not be null");
		int counter = 0;
		for (Resource resource : resources) {
			counter += loadBeanDefinitions(resource);
		}
		return counter;
	}

	@Override
	public int loadBeanDefinitions(String location) throws BeanDefinitionStoreException {
		ResourceLoader resourceLoader = getResourceLoader();
		if (resourceLoader == null) {
			throw new BeanDefinitionStoreException(String.format(
				"Cannot import bean definitions from location [%s]: no ResourceLoader available", location));
		}
		
		if (resourceLoader instanceof ResourcePatternResolver) {
			try {
				Resource[] resources = ((ResourcePatternResolver) resourceLoader).getResources(location);
				int loadCount = loadBeanDefinitions(resources);
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Loaded %s bean definitions from location pattern [%s]", loadCount, location));
				}
				return loadCount;
			}
			catch (IOException ex) {
				throw new BeanDefinitionStoreException(String.format(
					"Could not resolve bean definition resource pattern [%s]", location), ex);
			}
		}
		else {
			Resource resource = resourceLoader.getResource(location);
			int loadCount = loadBeanDefinitions(resource);
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("Loaded %s bean definitions from location [%s]", loadCount, location));
			}
			return loadCount;
		}
	}

	@Override
	public int loadBeanDefinitions(String[] locations) throws BeanDefinitionStoreException {
		Assert.notNull(locations, "Location array must not be null");
		int counter = 0;
		for (String location : locations) {
			counter += loadBeanDefinitions(location);
		}
		return counter;
	}
}
