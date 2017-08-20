package org.yuan.study.spring.core.io.support;

import java.io.IOException;

import org.yuan.study.spring.core.io.Resource;
import org.yuan.study.spring.core.io.ResourceLoader;

public interface ResourcePatternResolver extends ResourceLoader {

	/** in favor of CLASSPATH_ALL_URL_PREFIX */
	String CLASSPATH_ALL_URL_PREFIX = "classpath:";
	
	/** Pseudo URL prefix for all matching resource from the class path: "classpath*:" */
	String CLASSPATH_URL_PREFIX = "classpath*:";
	
	/**
	 * Resolve the given location pattern into Resource objects.
	 * @param locationPattern
	 * @return
	 */
	Resource[] getResources(String locationPattern) throws IOException;
}
