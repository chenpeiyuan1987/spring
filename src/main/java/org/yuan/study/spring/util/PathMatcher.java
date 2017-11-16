package org.yuan.study.spring.util;

import java.util.Comparator;
import java.util.Map;

public interface PathMatcher {

	/**
	 * Does the given path represent a pattern that can be matched
	 * by an implementation of this interface?
	 * @param str
	 * @return
	 */
	boolean isPattern(String path);
	
	/**
	 * Match the given path against the given pattern,
	 * according to this PathMatcher's matching strategy.
	 * @param pattern
	 * @param str
	 * @return
	 */
	boolean match(String pattern, String path);
	
	/**
	 * Match the given path against the corresponding part of the given pattern, 
	 * according to this PathMatcher's matching strategy.
	 * @param pattern
	 * @param path
	 * @return
	 */
	boolean matchStart(String pattern, String path);
	
	/**
	 * Given a pattern and a full path, determine the pattern-mapped part.
	 * @param pattern
	 * @param path
	 * @return
	 */
	String extractPathWithinPattern(String pattern, String path);
	
	/**
	 * Given a pattern and a full path, extract the URI template variables.
	 * URI template variables are expressed through curly brackets ('{' and '}');
	 * @param pattern
	 * @param path
	 * @return
	 */
	Map<String, String> extractUriTemplateVariables(String pattern, String path);
	
	/**
	 * Given a full path, returns a Comparator suitable for sorting patterns
	 * in order of explicitness for that path.
	 * @param path
	 * @return
	 */
	Comparator<String> getPatternComparator(String path);
	
	/**
	 * Combines two patterns into a new pattern that is returned.
	 * @param pattern1
	 * @param pattern2
	 * @return
	 */
	String combine(String pattern1, String pattern2);
}
