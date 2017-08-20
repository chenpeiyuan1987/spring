package org.yuan.study.spring.core.io.support;

import org.yuan.study.spring.util.ResourceUtils;


public abstract class ResourcePatternUtils {

	/**
	 * Return whether the given resource location is a URL:
	 * either a special "classpath" or "classpath*" pseudo URL or a standard URL.
	 * @param url
	 * @return
	 */
	public static boolean isUrl(String url) {
		if (url.startsWith(ResourcePatternResolver.CLASSPATH_ALL_URL_PREFIX)) {
			return true;
		}
		return ResourceUtils.isUrl(url);
	}
}
