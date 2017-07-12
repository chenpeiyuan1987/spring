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
		
		return (String[]) collection.toArray(new String[collection.size()]);
	}
	
}
