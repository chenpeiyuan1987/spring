package org.yuan.study.spring.core.io;

import org.yuan.study.spring.util.ResourceUtils;

public interface ResourceLoader {

	/** Pseudo URL prefix for loading from the class path: "classpath:" */
	String CLASSPATH_URL_PREFIX = ResourceUtils.CLASSPATH_URL_PREFIX;
	
	/**
	 * Return a Resource handle for the specified resource.
	 * @param location
	 * @return
	 */
	Resource getResource(String location);
}
