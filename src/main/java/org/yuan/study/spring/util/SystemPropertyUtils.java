package org.yuan.study.spring.util;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public abstract class SystemPropertyUtils {

	private static final Log logger = LogFactory.getLog(SystemPropertyUtils.class);
	
	private static final String PLACEHOLDER_PREFIX = "${";
	
	private static final String PLACEHOLDER_SUFFIX = "}";
	
	/**
	 * Resolve ${...} placeholders in the given text,
	 * replacing them with corresponding system property values.
	 * @param text
	 * @return
	 */
	public static String resolvePlaceholders(String text) {
		StringBuffer sb = new StringBuffer(text);
		
		int startIndex = text.indexOf(PLACEHOLDER_PREFIX);
		while (startIndex != -1) {
			int finisIndex = sb.toString().indexOf(PLACEHOLDER_SUFFIX, startIndex + PLACEHOLDER_PREFIX.length());
			if (finisIndex != -1) {
				String placeholder = sb.substring(startIndex + PLACEHOLDER_PREFIX.length(), finisIndex);
				String propVal = System.getProperty(placeholder);
				if (propVal != null) {
					sb.replace(startIndex, finisIndex + PLACEHOLDER_SUFFIX.length(), propVal);
					startIndex = sb.toString().indexOf(PLACEHOLDER_PREFIX, startIndex + propVal.length());
				}
				else {
					if (logger.isWarnEnabled()) {
						logger.warn(String.format("Could not resolve placeholder '%s' in [%s] as system property", placeholder, text));
					}
					startIndex = sb.toString().indexOf(PLACEHOLDER_PREFIX, finisIndex + PLACEHOLDER_SUFFIX.length());
				}
			} 
			else {
				startIndex = -1;
			}
		}
		
		return sb.toString();
	}
}
