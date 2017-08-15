package org.yuan.study.spring.util;

public abstract class ObjectUtils {
	
	/**
	 * Determine if the given objects are equal, 
	 * returning true if both are null or false if only one is null.
	 * @param o1
	 * @param o2
	 * @return
	 */
	public static boolean nullSafeEquals(Object o1, Object o2) {
		return (o1 == o2 || (o1 != null && o1.equals(o2)));
	}
	
	/**
	 * Return whether the given array is empty: that is, null or of zero length.
	 * @param array
	 * @return
	 */
	public static boolean isEmpty(Object[] array) {
		return (array == null || array.length == 0);
	}
	
	/**
	 * Return a hex string form of an object's identity hash code.
	 * @param obj
	 * @return
	 */
	public static String getIdentityHexString(Object obj) {
		return Integer.toHexString(System.identityHashCode(obj));
	}
}
