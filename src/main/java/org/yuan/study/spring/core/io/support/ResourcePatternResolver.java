package org.yuan.study.spring.core.io.support;

import java.io.IOException;

import org.yuan.study.spring.core.io.Resource;
import org.yuan.study.spring.core.io.ResourceLoader;

public interface ResourcePatternResolver extends ResourceLoader {

	/**  */
	String CLASSPATH_ALL_URL_PREFIX = "";
	
	/**  */
	String CLASSPATH_URL_PREFIX = "";
	
	/**
	 * 
	 * @param locationPattern
	 * @return
	 */
	Resource[] getResources(String locationPattern) throws IOException;
}
