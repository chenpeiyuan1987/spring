package org.yuan.study.spring.core.io;

import org.yuan.study.spring.util.ClassUtils;

public class DefaultResourceLoader implements ResourceLoader {
	
	private ClassLoader classLoader;
	
	/**
	 * 
	 */
	public DefaultResourceLoader() {
		this.classLoader = ClassUtils.getDefaultClassLoader();
	}

	/**
	 * 
	 * @param classLoader
	 */
	public DefaultResourceLoader(ClassLoader classLoader) {
		this.classLoader = classLoader;
	}

	public ClassLoader getClassLoader() {
		return classLoader;
	}

	public void setClassLoader(ClassLoader classLoader) {
		this.classLoader = classLoader;
	}
	
	protected Resource getResourceByPath(String path) {
		// TODO
		return null;
	}

	@Override
	public Resource getResource(String location) {
		// TODO Auto-generated method stub
		return null;
	}

}
