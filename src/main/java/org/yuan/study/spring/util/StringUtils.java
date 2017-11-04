package org.yuan.study.spring.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeSet;

public abstract class StringUtils {
	
	private static final String FOLDER_SEPARATOR = "/";
	
	private static final String WINDOWS_FOLDER_SEPARATOR = "\\";
	
	private static final String TOP_PATH = "..";
	
	private static final String CURRENT_PATH = ".";
	
	private static final char EXTENSION_SEPARATOR = '.';

	/**
	 * Convenience method to return a Collection as a CSV String.
	 * @param collection
	 * @return
	 */
	public static String collectionToCommaDelimitedString(Collection<?> collection) {
		return collectionToDelimitedString(collection, ",", "", "");
	}
	
	/**
	 * Convenience method to return a Collection as a delimited.
	 * @param collection
	 * @param delimiter
	 * @param prefix
	 * @param suffix
	 * @return
	 */
	public static String collectionToDelimitedString(Collection<?> collection, String delimiter, String prefix, String suffix) {
		if(CollectionUtils.isEmpty(collection)) {
			return "";
		}
		
		StringBuffer sb = new StringBuffer();
		Iterator<?> it = collection.iterator();
		while(it.hasNext()) {
			sb.append(prefix).append(it.next()).append(suffix);
			if(it.hasNext()) {
				sb.append(delimiter);
			}
		}
		return sb.toString();
	}
	
	/**
	 * Convenience method to return a Collection as a delimited.
	 * @param collection
	 * @param delimiter
	 * @param prefix
	 * @param suffix
	 * @return
	 */
	public static String collectionToDelimitedString(Collection<?> collection, String delimiter) {
		return collectionToDelimitedString(collection, delimiter, "", "");
	}
	
	/**
	 * Copy the given Collection into a String array.
	 * The Collection must contain String elements only.
	 * @param collection
	 * @return
	 */
	public static String[] toStringArray(Collection<String> collection) {
		if (collection == null) {
			return null;
		}
		return collection.toArray(new String[collection.size()]);
	}
	
	/**
	 * Check whether the given String has actual text.
	 * @param str
	 * @return
	 */
	public static boolean hasText(CharSequence str) {
		if (!hasLength(str)) {
			return false;
		}
		
		int strLen = str.length();
		for (int i = 0; i < strLen; i++) {
			if (!Character.isWhitespace(str.charAt(i))) {
				return true;
			}
		}
		
		return false;
	}
	
	/**
	 * Check whether the given String has actual text.
	 * @param str
	 * @return
	 */
	public static boolean hasText(String str) {
		return hasText((CharSequence)str);
	}
	
	/**
	 * Check whether the given String contains any whitespace characters.
	 * @param str
	 * @return
	 */
	public static boolean containsWhitespace(CharSequence str) {
		if (!hasLength(str)) {
			return false;
		}
		
		int strLen = str.length();
		for (int i = 0; i < strLen; i++) {
			if (Character.isWhitespace(str.charAt(i))) {
				return true;
			}
		}
		
		return false;
	}
	
	/**
	 * Check whether the given String contains any whitespace characters.
	 * @param str
	 * @return
	 */
	public static boolean containsWhitespace(String str) {
		return containsWhitespace((CharSequence)str);
	}
	
	/**
	 * Trim leading and trailing whitespace from the given String.
	 * @param str
	 * @return
	 */
	public static String trimWhitespace(String str) {
		if (!hasLength(str)) {
			return str;
		}
		
		StringBuilder sb = new StringBuilder(str);
		while (sb.length() > 0 && Character.isWhitespace(sb.charAt(0))) {
			sb.deleteCharAt(0);
		}
		while (sb.length() > 0 && Character.isWhitespace(sb.charAt(sb.length() - 1))) {
			sb.deleteCharAt(sb.length() - 1);
		}
		
		return sb.toString();
	}
	
	/**
	 * Trim all whitespace from the given String:
	 * leading, trailing, and inbetween characters.
	 * @param str
	 * @return
	 */
	public static String trimAllWhitespace(String str) {
		if (!hasLength(str)) {
			return str;
		}
		
		StringBuilder sb = new StringBuilder(str);
		int index = 0;
		while (sb.length() > index) {
			if (Character.isWhitespace(sb.charAt(index))) {
				sb.deleteCharAt(index);
			} 
			else {
				index++;
			}
		}
		
		return sb.toString();
	}
	
	/**
	 * Trim leading whitespace from the given String.
	 * @param str
	 * @return
	 */
	public static String trimLeadingWhitespace(String str) {
		if (!hasLength(str)) {
			return str;
		}
		
		StringBuilder sb = new StringBuilder(str);
		while (sb.length() > 0 && Character.isWhitespace(sb.charAt(0))) {
			sb.deleteCharAt(0);
		}
		
		return sb.toString();
	}
	
	/**
	 * Trim trailing whitespace from the given String.
	 * @param str
	 * @return
	 */
	public static String trimTrailingWhitespace(String str) {
		if (!hasLength(str)) {
			return str;
		}
		
		StringBuilder sb = new StringBuilder(str);
		while (sb.length() > 0 && Character.isWhitespace(sb.charAt(sb.length() - 1))) {
			sb.deleteCharAt(sb.length() - 1);
		}
		
		return sb.toString();
	}
	
	/**
	 * Trim all occurences of the supplied leading character from the given String.
	 * @param str
	 * @return
	 */
	public static String trimLeadingCharacter(String str, char ch) {
		if (!hasLength(str)) {
			return str;
		}
		
		StringBuilder sb = new StringBuilder(str);
		while (sb.length() > 0 && sb.charAt(0) == ch) {
			sb.deleteCharAt(0);
		}
		
		return sb.toString();
	}
	
	/**
	 * Trim all occurences of the supplied trailing character from the given String.
	 * @param str
	 * @return
	 */
	public static String trimTrailingCharacter(String str, char ch) {
		if (!hasLength(str)) {
			return str;
		}
		
		StringBuilder sb = new StringBuilder(str);
		while (sb.length() > 0 && sb.charAt(sb.length() - 1) == ch) {
			sb.deleteCharAt(sb.length() - 1);
		}
		
		return sb.toString();
	}
	
	/**
	 * Test if the given String starts with the specified prefix,
	 * ignoring upper/lower case.
	 * @param str
	 * @param prefix
	 * @return
	 */
	public static boolean startsWithIgnoreCase(String str, String prefix) {
		if (str == null || prefix == null) {
			return false;
		}
		if (str.length() < prefix.length()) {
			return false;
		}
		if (str.startsWith(prefix)) {
			return true;
		}
		String lcStr = str.substring(0, prefix.length()).toLowerCase();
		String lcPrefix = prefix.toLowerCase();
		return lcStr.equals(lcPrefix);
	}
	
	/**
	 * Test if the given String ends with the specified suffix,
	 * ignoring uppper/lower case.
	 * @param str
	 * @param suffix
	 * @return
	 */
	public static boolean endsWithIgnoreCase(String str, String suffix) {
		if (str == null || suffix == null) {
			return false;
		}
		if (str.length() < suffix.length()) {
			return false;
		}
		if (str.endsWith(suffix)) {
			return true;
		}
		String lcStr = str.substring(str.length() - suffix.length()).toLowerCase();
		String lcSuffix = suffix.toLowerCase();
		return lcStr.equals(lcSuffix);
	}
	
	/**
	 * Test whether the given string matches the given substring at the given index.
	 * @param str
	 * @param index
	 * @param substring
	 * @return
	 */
	public static boolean substringMatch(CharSequence str, int index, CharSequence substring) {
		for (int i = 0; i < substring.length(); i++) {
			int j = index + i;
			if (j >= str.length() || str.charAt(j) != substring.charAt(i)) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * Delete all occurrences of the given substring. 
	 * @param inString
	 * @param pattern
	 * @return
	 */
	public static String delete(String inString, String pattern) {
		return replace(inString, pattern, "");
	}
	
	/**
	 * Check that the given String is neither null nor of length 0.
	 * @param str
	 * @return
	 */
	public static boolean hasLength(String str) {
		return hasLength((CharSequence)str);
	}
	
	/**
	 * Check that the given String is neither null nor of length 0.
	 * @param str
	 * @return
	 */
	public static boolean hasLength(CharSequence str) {
		return (str != null && str.length() > 0);
	}
	
	/**
	 * Convenience method to return a String array as a delimited 
	 * @param arr
	 * @param delim
	 * @return
	 */
	public static String arrayToDelimitedString(Object[] arr, String delim) {
		if (ObjectUtils.isEmpty(arr)) {
			return "";
		}
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < arr.length; i++) {
			if (i > 0) {
				sb.append(delim);
			}
			sb.append(arr[i]);
		}
		return sb.toString();
	}
	
	/**
	 * Tokenize the given String into a String array via a StringTokenizer.
	 * @param str
	 * @param delimiters
	 * @return
	 */
	public static String[] tokenizeToStringArray(String str, String delimiters) {
		return tokenizeToStringArray(str, delimiters, true, true);
	}
	
	/**
	 * Tokenize the given String into a String array via a StringTokenizer.
	 * @param str
	 * @param delimiters
	 * @param trimTokens
	 * @param ignoreEmptyTokens
	 * @return
	 */
	public static String[] tokenizeToStringArray(String str, String delimiters, boolean trimTokens, boolean ignoreEmptyTokens) {
		if (str == null) {
			return null;
		}
		StringTokenizer st = new StringTokenizer(str, delimiters);
		List<String> tokens = new ArrayList<String>();
		while (st.hasMoreTokens()) {
			String token = st.nextToken();
			if (trimTokens) {
				token = token.trim();
			}
			if (!ignoreEmptyTokens || token.length() > 0) {
				tokens.add(token);
			}
		}
		return toStringArray(tokens);
	}
	
	/**
	 * Parse the given locale string into a java.util.Locale.
	 * @param localString
	 * @return
	 */
	public static Locale parseLocaleString(String localString) {
		String[] parts = tokenizeToStringArray(localString, "_ ", false, false);
		String language = (parts.length > 0 ? parts[0] : "");
		String country = (parts.length > 1 ? parts[1] : "");
		String variant = (parts.length > 2 ? parts[2] : "");
		return (language.length() > 0 ? new Locale(language, country, variant) : null);
	}
	
	/**
	 * Take a String which is a delimited list and convert it to a String array.
	 * @param str
	 * @param delimiter
	 * @return
	 */
	public static String[] delimitedListToStringArray(String str, String delimiter) {
		if (str == null) {
			return new String[0];
		}
		if (delimiter == null) {
			return new String[]{str};
		}
		List<String> result = new ArrayList<String>();
		if ("".equals(delimiter)) {
			for (int i = 0; i < str.length(); i++) {
				result.add(str.substring(i, i+1));
			}
		} 
		else {
			int pos = 0;
			int delPos = 0;
			while ((delPos = str.indexOf(delimiter, pos)) != -1) {
				result.add(str.substring(pos, delPos));
				pos = delPos + delimiter.length();
			}
			if (str.length() > 0 && pos <= str.length()) {
				result.add(str.substring(pos));
			}
		}
		return toStringArray(result);
	}
	
	/**
	 * 
	 * @param str
	 * @param delimiter
	 * @param charsToDelete
	 * @return
	 */
	public static String[] delimitedListToStringArray(String str, String delimiter, String charsToDelete) {
		
	}
	
	/**
	 * Normalize the path by suppressing sequences like "path/.." and inner simple dots.
	 * @param path
	 * @return
	 */
	public static String cleanPath(String path) {
		String pathToUse = replace(path, WINDOWS_FOLDER_SEPARATOR, FOLDER_SEPARATOR);
		
		int index = pathToUse.indexOf(":");
		String prefix = "";
		if (index != -1) {
			prefix = pathToUse.substring(0, index + 1);
			if (prefix.contains("/")) {
				prefix = "";
			} 
			else {
				pathToUse = pathToUse.substring(index + 1);
			}
		}
		if (pathToUse.startsWith(FOLDER_SEPARATOR)) {
			prefix = prefix + FOLDER_SEPARATOR;
			pathToUse = pathToUse.substring(1);
		}
		
		String[] pathArray = delimitedListToStringArray(pathToUse, FOLDER_SEPARATOR);
		List<String> pathElements = new LinkedList<String>();
		int tops = 0;
		
		for (int i = pathArray.length - 1; i >= 0; i--) {
			String item = pathArray[i];
			if (CURRENT_PATH.equals(item)) {
				
			} 
			else if (TOP_PATH.equals(item)) {
				tops++;
			}
			else {
				if (tops > 0) {
					tops--;
				} 
				else {
					pathElements.add(0, item);
				}
			}
		}
		
		for (int i = 0; i < tops; i++) {
			pathElements.add(0, TOP_PATH);
		}
		
		return prefix + collectionToDelimitedString(pathElements, FOLDER_SEPARATOR);
	}
	
	/**
	 * Replace all occurences of a substring within a string with another string.
	 * @param inString
	 * @param oldPattern
	 * @param newPattern
	 * @return
	 */
	public static String replace(String inString, String oldPattern, String newPattern) {
		if (!hasLength(inString) || !hasLength(oldPattern) || newPattern == null) {
			return inString;
		}
		
		StringBuffer sb = new StringBuffer();
		int pos = 0;
		int index = inString.indexOf(oldPattern);
		int patLen = oldPattern.length();
		while (index >= 0) {
			sb.append(inString.substring(pos, index));
			sb.append(newPattern);
			pos = index + patLen;
			index = inString.indexOf(oldPattern, pos);
		}
		sb.append(inString.substring(pos));
		
		return sb.toString();
	}
	
	/**
	 * Apply the given relative path to the given path,
	 * assuming standard Java folder separation.
	 * @param path
	 * @param relativePath
	 * @return
	 */
	public static String applyRelativePath(String path, String relativePath) {
		int index = path.lastIndexOf(FOLDER_SEPARATOR);
		if (index != -1) {
			String newPath = path.substring(0, index);
			if (!relativePath.startsWith(FOLDER_SEPARATOR)) {
				newPath += FOLDER_SEPARATOR;
			}
			return newPath + relativePath;
		}
		return relativePath;
	}
	
	/**
	 * Convenience method to return a String array as a CSV String.
	 * @param convertedValue
	 * @return
	 */
	public static String arrayToCommaDelimitedString(Object[] arr) {
		return arrayToDelimitedString(arr, ",");
	}
	
	
	/**
	 * Extract the filename from the given path.
	 * @return
	 */
	public static String getFilename(String path) {
		if (path == null) {
			return null;
		}
		int index = path.lastIndexOf(FOLDER_SEPARATOR);
		return (index != -1 ? path.substring(index + 1) : path);
	}
	
	/**
	 * Extract the filename extension from the given path,
	 * e.g. "mypath/myfile.txt" -> "txt".
	 * @param path
	 * @return
	 */
	public static String getFilenameExtension(String path) {
		if (path == null) {
			return null;
		}
		
		int extIndex = path.lastIndexOf(EXTENSION_SEPARATOR);
		if (extIndex == -1) {
			return null;
		}
		int dirIndex = path.lastIndexOf(FOLDER_SEPARATOR);
		if (dirIndex > extIndex) {
			return null;
		}
		return path.substring(extIndex + 1);
	}
	
	/**
	 * Count the occurrences of the substring in string s.
	 * @param str
	 * @param sub
	 * @return
	 */
	public static int countOccurrencesOf(String str, String sub) {
		if (str == null || str.length() == 0) {
			return 0;
		}
		if (sub == null || sub.length() == 0) {
			return 0;
		}
		
		int count = 0;
		int index = 0;
		int place = 0;
		while ((index = str.indexOf(sub, place)) != -1) {
			count++;
			place = index + sub.length();
		}
		return count;
	}
	
	/**
	 * Delete any character in a given String.
	 * @param text
	 * @param deleteChars
	 * @return
	 */
	public static String deleteAny(String text, String deleteChars) {
		if (!hasLength(text) || !hasLength(deleteChars)) {
			return text;
		}
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < text.length(); i++) {
			char ch = text.charAt(i);
			if (deleteChars.indexOf(ch) == -1) {
				sb.append(ch);
			}
		}
		return sb.toString();
	}
	
	/**
	 * Capitalize a String, changing the first letter to upper case.
	 * @param str
	 * @return
	 */
	public static String capitalize(String str) {
		return changeFirstCharacterCase(str, true);
	}
	
	/**
	 * Uncapitalize a String, changing the first letter to lower case.
	 * @param str
	 * @return
	 */
	public static String uncapitalize(String str) {
		return changeFirstCharacterCase(str, false);
	}
	
	private static String changeFirstCharacterCase(String str, boolean capitalize) {
		if (str == null || str.length() == 0) {
			return str;
		}
		StringBuilder sb = new StringBuilder(str.length());
		if (capitalize) {
			sb.append(Character.toUpperCase(str.charAt(0)));
		}
		else {
			sb.append(Character.toLowerCase(str.charAt(0)));
		}
		sb.append(str.substring(0));
		return sb.toString();
	}
	
	/**
	 * 
	 * @param str
	 * @return
	 */
	public static String[] commaDelimitedListToStringArray(String str) {
		if (!StringUtils.hasText(str)) {
			return new String[0];
		}
		return str.split(",");
	}
	
	/**
	 * 
	 * @param str
	 * @return
	 */
	public static Set<String> commaDelimitedListToSet(String str) {
		Set<String> set = new TreeSet<String>();
		String[] tokens = commaDelimitedListToStringArray(str);
		for (String token : tokens) {
			set.add(token);
		}
		return set;
	}
	
	/**
	 * Quote the given String with single quotes.
	 * @param str
	 * @return
	 */
	public static String quote(String str) {
		return (str != null ? "'" + str + "'" : null);
	}
	
	/**
	 * Turn the given Object into a String with single quotes
	 * if it is a String; keeping the Object as-is else.
	 * @param obj
	 * @return
	 */
	public static Object quoteIfString(Object obj) {
		return (obj instanceof String ? quote((String) obj) : obj);
	}
	
	/**
	 * Unqualify a string qualified by a '.' dot character. For example,
	 * "this.name.is.qualified", returns "qualified".
	 * @param qualifiedName
	 * @return
	 */
	public static String unqualify(String qualifiedName) {
		return unqualify(qualifiedName, '.');
	}
	
	/**
	 * Unqualify a string qualified by a separator character. For example,
	 * "this.name:is:qualified" returns "qualified" if using a ':' separator.
	 * @param qualifiedName
	 * @param separator
	 * @return
	 */
	public static String unqualify(String qualifiedName, char separator) {
		return qualifiedName.substring(qualifiedName.lastIndexOf(separator) + 1);
	}
}
