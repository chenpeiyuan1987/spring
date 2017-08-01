package org.yuan.study.spring.util;

import java.util.Collection;
import java.util.Iterator;

public abstract class StringUtils {

	/**
	 * 
	 * @param collection
	 * @return
	 */
	public static String collectionToCommaDelimitedString(Collection<?> collection) {
		return collectionToDelimitedString(collection, ",", "", "");
	}
	
	/**
	 * 
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
	 * 
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
	public static boolean hasText(String str) {
		if (!hasLength(str)) {
			return false;
		}
		
		for (char ch : str.toCharArray()) {
			if (!Character.isWhitespace(ch)) {
				return true;
			}
		}
		
		return false;
	}
	
	/**
	 * Check that the given String is neither null nor of length 0.
	 * @param str
	 * @return
	 */
	public static boolean hasLength(String str) {
		return (str != null && str.length() > 0);
	}
	
	/**
	 * 
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
}
