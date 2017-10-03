package org.yuan.study.spring.util;

import java.util.Collection;
import java.util.Map;

public abstract class CollectionUtils {
	
	/**
	 * Return true if the supplied Collection is null or empty. 
	 * Otherwise, return false.
	 * @param collection
	 * @return
	 */
	public static boolean isEmpty(Collection<?> collection) {
		return (collection == null || collection.isEmpty());
	}
	
	/**
	 * Return true if the supplied Map is null or empty. 
	 * Otherwise, return false.
	 * @param collection
	 * @return
	 */
	public static boolean isEmpty(Map<?,?> map) {
		return (map == null || map.isEmpty());
	}
}
