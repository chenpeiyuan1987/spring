package org.yuan.study.spring.core.io;

import java.net.MalformedURLException;
import java.net.URL;

import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ClassUtils;

public class DefaultResourceLoader implements ResourceLoader {
	
	private ClassLoader classLoader;
	
	/**
	 * Create a new DefaultResourceLoader.
	 */
	public DefaultResourceLoader() {
		this.classLoader = ClassUtils.getDefaultClassLoader();
	}

	/**
	 * Create a new DefaultResourceLoader.
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
		return new ClassPathResource(path, getClassLoader());
	}

	@Override
	public Resource getResource(String location) {
		Assert.notNull(location, "Location must not be null");
		if (location.startsWith(CLASSPATH_URL_PREFIX)) {
			return new ClassPathResource(location.substring(CLASSPATH_URL_PREFIX.length()), getClassLoader());
		}
		else {
			try {
				URL url = new URL(location);
				return new UrlResource(url);
			}
			catch (MalformedURLException ex) {
				return getResourceByPath(location);
			}
		}
	}

}
