package org.yuan.study.spring.core.io.support;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.util.AntPathMatcher;
import org.springframework.util.PathMatcher;
import org.yuan.study.spring.core.io.DefaultResourceLoader;
import org.yuan.study.spring.core.io.Resource;
import org.yuan.study.spring.core.io.ResourceLoader;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ClassUtils;

public class PathMatchingResourcePatternResolver implements ResourcePatternResolver {

	protected final Log logger = LogFactory.getLog(getClass());
	
	private final ResourceLoader resourceLoader;
	
	private ClassLoader classLoader;
	
	private PathMatcher pathMatcher = new AntPathMatcher();
	
	public PathMatchingResourcePatternResolver() {
		this(new DefaultResourceLoader(), null);
	}

	public PathMatchingResourcePatternResolver(ClassLoader classLoader) {
		this(new DefaultResourceLoader(classLoader), classLoader);
	}

	public PathMatchingResourcePatternResolver(ResourceLoader resourceLoader) {
		this(resourceLoader, null);
	}

	public PathMatchingResourcePatternResolver(ResourceLoader resourceLoader, ClassLoader classLoader) {
		Assert.notNull(resourceLoader, "ResourceLoader must not be null");
		this.resourceLoader = resourceLoader;
		this.classLoader = (classLoader != null ? classLoader : ClassUtils.getDefaultClassLoader());
	}
	
	public ResourceLoader getResourceLoader() {
		return resourceLoader;
	}
	
	public PathMatcher getPathMatcher() {
		return pathMatcher;
	}
	
	public Resource[] findPathMatchingResources(String locationPattern) {
		return null;
	}
	
	public Resource[] findAllClassPathResources(String locationPattern) {
		return null;
	}
	
	@Override
	public Resource getResource(String location) {
		return getResourceLoader().getResource(location);
	}

	@Override
	public Resource[] getResources(String locationPattern) {
		Assert.notNull(locationPattern, "Location pattern must not be null");
		if (locationPattern.startsWith(CLASSPATH_ALL_URL_PREFIX)) {
			if (getPathMatcher().isPattern(locationPattern.substring(CLASSPATH_ALL_URL_PREFIX.length()))) {
				return findPathMatchingResources(locationPattern);
			} 
			else {
				return findAllClassPathResources(locationPattern.substring(CLASSPATH_ALL_URL_PREFIX.length()));
			}
		} 
		else {
			int index = locationPattern.indexOf(":") + 1;
			if (getPathMatcher().isPattern(locationPattern.substring(index))) {
				return findPathMatchingResources(locationPattern);
			} 
			else {
				return new Resource[] {getResourceLoader().getResource(locationPattern)};
			}
		}
		
	}

}
