package org.yuan.study.spring.util;

public interface PathMatcher {

	/**
	 * Return if the given string represents a pattern to be matched
	 * via this class: If not, the "match" method does not have to be 
	 * used because direct equality comparisons  are sufficient.
	 * @param str
	 * @return
	 */
	boolean isPattern(String str);
	
	/**
	 * Match a string against the given pattern.
	 * @param pattern
	 * @param str
	 * @return
	 */
	boolean match(String pattern, String str);
	
}
