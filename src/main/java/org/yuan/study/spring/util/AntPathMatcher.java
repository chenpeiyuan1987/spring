package org.yuan.study.spring.util;

import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class AntPathMatcher implements PathMatcher {
	
	private static final Pattern VARIABLE_PATTERN = Pattern.compile("\\{[^/]+?\\}");
	
	public static final String DEFAULT_PATH_SEPARATOR = "/";
	
	private String pathSeparator = DEFAULT_PATH_SEPARATOR;
	
	/**
	 * Set the path separator to use for pattern parsing. Default is "/", as in Ant.
	 * @param pathSeparator
	 */
	public void setPathSeparator(String pathSeparator) {
		if (pathSeparator != null) {
			this.pathSeparator = pathSeparator;
		}
		else {
			this.pathSeparator = DEFAULT_PATH_SEPARATOR;
		}
	}
	
	@Override
	public boolean isPattern(String path) {
		return (path.indexOf('*') != -1 || path.indexOf('?') != -1);
	}
	
	@Override
	public boolean match(String pattern, String path) {
		return doMatch(pattern, path, true, null);
	}

	@Override
	public boolean matchStart(String pattern, String path) {
		return doMatch(pattern, path, false, null);
	}

	@Override
	public String extractPathWithinPattern(String pattern, String path) {
		String[] patternParts = StringUtils.tokenizeToStringArray(pattern, pathSeparator);
		String[] pathParts = StringUtils.tokenizeToStringArray(path, pathSeparator);
		
		StringBuilder builder = new StringBuilder();
		
		int puts = 0;
		for (int i = 0; i < patternParts.length; i++) {
			String patternPart = patternParts[i];
			if ((patternPart.indexOf('*') > -1 || patternPart.indexOf('?') > -1) && pathParts.length >= i + 1) {
				if (puts > 0 || (i == 0 && !pattern.startsWith(pathSeparator))) {
					builder.append(pathSeparator);
				}
				builder.append(pathParts[i]);
				puts++;
			}
		}
		
		for (int i = patternParts.length; i < pathParts.length; i++) {
			if (puts > 0 || i > 0) {
				builder.append(pathSeparator);
			}
			builder.append(pathParts[i]);
		}
		
		return builder.toString();
	}

	@Override
	public Map<String, String> extractUriTemplateVariables(String pattern, String path) {
		Map<String, String> variables = new LinkedHashMap<String, String>();
		boolean result = doMatch(pattern, path, true, variables);
		Assert.state(result, String.format("Pattern '%s' is not a match for '%s'", pattern, path));
		return variables;
	}

	@Override
	public Comparator<String> getPatternComparator(String path) {
		return new AntPatternComparator(path);
	}

	@Override
	public String combine(String pattern1, String pattern2) {
		if (!StringUtils.hasText(pattern1) && !StringUtils.hasText(pattern2)) {
			return "";
		}
		if (!StringUtils.hasText(pattern1)) {
			return pattern2;
		}
		if (!StringUtils.hasText(pattern2)) {
			return pattern1;
		}
		if (match(pattern1, pattern2)) {
			return pattern2;
		}
		if (pattern1.endsWith("/*")) {
			if (pattern2.startsWith("/")) {
				// /path1/* + /path2 -> /path1/path2
				return pattern1.substring(0, pattern1.length() - 1) + pattern2.substring(1);
			}
			else {
				// /path1/* + path2 -> /path1/path2
				return pattern1.substring(0, pattern1.length() - 1) + pattern2;
			}
		}
		if (pattern1.endsWith("/**")) {
			if (pattern2.startsWith("/")) {
				// /path1/** + /path2 -> /path1/**/path2
				return pattern1 + pattern2;
			}
			else {
				// /path1/** + path2 -> /path1/**/path2
				return pattern1 + "/" + pattern2;
			}
		}
		
		int dotPos1 = pattern1.indexOf('.');
		if (dotPos1 == -1) {
			if (pattern1.endsWith("/") || pattern2.startsWith("/")) {
				return pattern1 + pattern2;
			}
			else {
				return pattern1 + "/" + pattern2;
			}
		}
		String fileName1 = pattern1.substring(0, dotPos1);
		String extension1 = pattern1.substring(dotPos1);
		String fileName2;
		String extension2;
		int dotPos2 = pattern2.indexOf('.');
		if (dotPos2 != -1) {
			fileName2 = pattern2.substring(0, dotPos2);
			extension2 = pattern2.substring(dotPos2);
		}
		else {
			fileName2 = pattern2;
			extension2 = "";
		}
		String fileName = fileName1.endsWith("*") ? fileName2 : fileName1;
		String extension = extension1.startsWith("*") ? extension2 : extension1;
		
		return fileName + extension;
	}

	/**
	 * Tests whether or not a string matches against a pattern.
	 * The pattern may contain two special characters:
	 * '*' means zero or more characters
	 * '?' means one and only one character
	 * @param pattern
	 * @param str
	 * @return
	 */
	private boolean matchStrings(String pattern, String str, Map<String, String> uriTemplateVariables) {
		return true;
	}

	protected boolean doMatch(String pattern, String path, boolean fullMatch, Map<String, String> uriTemplateVariables) {
		return true;
	}

	private static class AntPatternComparator implements Comparator<String> {
		
		private final String path;

		public AntPatternComparator(String path) {
			this.path = path;
		}

		@Override
		public int compare(String pattern1, String pattern2) {
			if (pattern1 == null && pattern2 == null) {
				return 0;
			}
			else if (pattern1 == null) {
				return 1;
			}
			else if (pattern2 == null) {
				return -1;
			}
			
			boolean pattern1EqualsPath = pattern1.equals(path);
			boolean pattern2EqualsPath = pattern2.equals(path);
			if (pattern1EqualsPath && pattern2EqualsPath) {
				return 0;
			}
			else if (pattern1EqualsPath) {
				return -1;
			}
			else if (pattern2EqualsPath) {
				return 1;
			}
			
			int wildCardCount1 = getWildCardCount(pattern1);
			int wildCardCount2 = getWildCardCount(pattern2);
			
			int bracketCount1 = StringUtils.countOccurrencesOf(pattern1, "{");
			int bracketCount2 = StringUtils.countOccurrencesOf(pattern2, "}");
			
			int totalCount1 = wildCardCount1 + bracketCount1;
			int totalCount2 = wildCardCount2 + bracketCount2;
			
			if (totalCount1 != totalCount2) {
				return totalCount1 - totalCount2;
			}
			
			int pattern1Length = getPatternLength(pattern1);
			int pattern2Length = getPatternLength(pattern2);
			
			if (pattern1Length != pattern2Length) {
				return pattern2Length - pattern1Length;
			}
			
			if (wildCardCount1 < wildCardCount2) {
				return -1;
			}
			else if (wildCardCount2 < wildCardCount1) {
				return 1;
			}
			
			if (bracketCount1 < bracketCount2) {
				return -1;
			}
			else if (bracketCount2 < bracketCount1) {
				return 1;
			}
			
			return 0;
		}
		
		private int getWildCardCount(String pattern) {
			if (pattern.endsWith(".*")) {
				pattern = pattern.substring(0, pattern.length() - 2);
			}
			return StringUtils.countOccurrencesOf(pattern, "*");
		}
		
		private int getPatternLength(String pattern) {
			Matcher m = VARIABLE_PATTERN.matcher(pattern);
			return m.replaceAll("#").length();
		}
	}
}
