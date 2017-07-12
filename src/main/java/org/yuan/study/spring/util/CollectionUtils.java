package org.yuan.study.spring.util;

import java.util.Collection;

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
}
