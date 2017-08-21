package org.yuan.study.spring.util;

public class AntPathMatcher implements PathMatcher {
	
	public static final String DEFAULT_PATH_SEPARATOR = "/";
	
	private String pathSeparator = DEFAULT_PATH_SEPARATOR;
	
	/**
	 * Set the path separator to use for pattern parsing.
	 * Default is "/", as in Ant.
	 * @param pathSeparator
	 */
	public void setPathSeparator(String pathSeparator) {
		if (pathSeparator != null) {
			this.pathSeparator = pathSeparator;
		}
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
	private boolean matchStrings(String pattern, String str) {
		char[] patArr = pattern.toCharArray();
		char[] strArr = str.toCharArray();
		int patIndexStart = 0;
		int patIndexFinis = patArr.length - 1;
		int strIndexStart = 0;
		int strIndexFinis = strArr.length - 1;
		char ch;
		
		boolean containsStar = false;
		for (int i = 0; i < patArr.length; i++) {
			if (patArr[i] == '*') {
				containsStar = true;
				break;
			}
		}
		
		if (!containsStar) {
			if (patIndexFinis != strIndexFinis) {
				return false;
			}
			for (int i = 0; i < patIndexFinis; i++) {
				ch = patArr[i];
				if (ch != '?') {
					if (ch != strArr[i]) {
						return false;
					}
				}
			}
			return true;
		}
		
		if (patIndexFinis == 0) {
			return true;
		}
		
		while ((ch = patArr[patIndexStart]) != '*' && strIndexStart <= strIndexFinis) {
			if (ch != '?') {
				if (ch != strArr[strIndexStart]) {
					return false;
				}
			}
			patIndexStart++;
			strIndexStart++;
		}
		if (strIndexStart > strIndexFinis) {
			for (int i = patIndexStart; i <= patIndexFinis; i++) {
				if (patArr[i] != '*') {
					return false;
				}
			}
			return true;
		}
		
		while ((ch = patArr[patIndexFinis]) != '*' && strIndexStart <= strIndexFinis) {
			if (ch != '?') {
				if (ch != strArr[strIndexFinis]) {
					return false;
				}
			}
			patIndexFinis--;
			strIndexFinis--;
		}
		if (strIndexStart > strIndexFinis) {
			for (int i = patIndexStart; i <= patIndexFinis; i++) {
				if (patArr[i] != '*') {
					return false;
				}
			}
			return true;
		}
		
		while (patIndexStart != patIndexFinis && strIndexStart <= strIndexFinis) {
			int patIndexTmp = -1;
			for (int i = patIndexStart; i <= patIndexFinis; i++) {
				if (patArr[i] == '*') {
					patIndexTmp = i;
					break;
				}
			}
			if (patIndexTmp == patIndexStart + 1) {
				patIndexStart++;
				continue;
			}
			
			int patLength = (patIndexTmp - patIndexStart - 1);
			int strLength = (strIndexFinis - strIndexStart + 1);
			int foundIndex = -1;
			strLoop:
			for (int i = 0; i <= strLength - patLength; i++) {
				for (int j = 0; j < patLength; j++) {
					ch = patArr[patIndexStart + j + 1];
					if (ch != '?') {
						if (ch != strArr[strIndexStart + i + j]) {
							continue strLoop;
						}
					}
				}
				
				foundIndex = strIndexStart + i;
				break;
			}
			
			if (foundIndex == -1) {
				return false;
			}
			
			patIndexStart = patIndexTmp;
			strIndexStart = foundIndex + patLength;
		}
		
		for (int i = patIndexStart; i <= patIndexFinis; i++) {
			if (patArr[i] != '*') {
				return false;
			}
		}
		
		return true;
	}

	@Override
	public boolean isPattern(String str) {
		return (str.indexOf('*') != -1 || str.indexOf('?') != -1);
	}

	@Override
	public boolean match(String pattern, String str) {
		if (str.startsWith(pathSeparator) != pattern.startsWith(pathSeparator)) {
			return false;
		}
		
		String[] patDirs = StringUtils.tokenizeToStringArray(pattern, pathSeparator);
		String[] strDirs = StringUtils.tokenizeToStringArray(str, pathSeparator);
		
		int patIndexStart = 0;
		int patIndexFinis = patDirs.length - 1;
		int strIndexStart = 0;
		int strIndexFinis = strDirs.length - 1;
		
		while (patIndexStart <= patIndexFinis && strIndexStart <= strIndexFinis) {
			String patDir = (String) patDirs[patIndexStart];
			if (patDir.equals("**")) {
				break;
			}
			if (!matchStrings(patDir, (String) strDirs[strIndexStart])) {
				return false;
			}
			patIndexStart++;
			strIndexStart++;
		}
		
		if (strIndexStart > strIndexFinis) {
			if (patIndexStart == patIndexFinis && patDirs[patIndexStart].equals("*") && str.endsWith("*")) {
				return true;
			}
			for (int i = patIndexStart; i < patIndexFinis; i++) {
				if (!patDirs[i].equals("**")) {
					return false;
				}
			}
			return true;
		} 
		else {
			if (strIndexStart > patIndexFinis) {
				return false;
			}
		}
		
		while (patIndexStart <= patIndexFinis && strIndexStart <= strIndexFinis) {
			String patDir = (String) patDirs[patIndexFinis];
			if (patDir.equals("**")) {
				break;
			}
			if (!matchStrings(patDir, (String) strDirs[strIndexFinis])) {
				return false;
			}
			patIndexFinis--;
			strIndexFinis--;
		}
		if (strIndexStart > strIndexFinis) {
			for (int i = patIndexStart; i < patIndexFinis; i++) {
				if (!patDirs[i].equals("**")) {
					return false;
				}
			}
			return true;
		}
		
		while (patIndexStart != patIndexFinis && strIndexStart <= strIndexFinis) {
			int patIndexTmp = -1;
			for (int i = patIndexStart + 1; i <= patIndexFinis; i++) {
				if (patDirs[i].equals("**")) {
					patIndexTmp = i;
					break;
				}
			}
			if (patIndexTmp == patIndexStart + 1) {
				patIndexStart++;
				continue;
			}
			
			int patLength = (patIndexTmp - patIndexStart - 1);
			int strLength = (strIndexFinis - strIndexStart + 1);
			int foundIndex = -1;
			for (int i=0; i<strLength - patLength; i++) {
				for (int j=0; j<patLength; j++) {
					String subPat = (String) patDirs[patIndexStart + j + 1];
					String subStr = (String) strDirs[strIndexStart + i + j];
					if (!matchStrings(subPat, subStr)) {
						
					}
				}
			}
			if (foundIndex == -1) {
				return false;
			}
			
			patIndexStart = patIndexTmp;
			strIndexStart = foundIndex + patLength;
		}
		
		for (int i = patIndexStart; i <= patIndexFinis; i++) {
			if (!patDirs[i].equals("**")) {
				return false;
			}
		}
		
		return true;
	}

}
