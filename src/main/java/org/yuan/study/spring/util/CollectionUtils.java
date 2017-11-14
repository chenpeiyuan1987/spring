package org.yuan.study.spring.util;

import java.awt.font.NumericShaper;
import java.util.Arrays;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

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
	
	/**
	 * Convert the supplied array into a List. A primitive array gets
	 * converted into a List of the appropriate wrapper type.
	 * A null source value will be converted to an empty List.
	 * @return
	 */
	public static List<Object> arrayToList(Object source) {
		return Arrays.asList(ObjectUtils.toObjectArray(source));
	}
	
	/**
	 * Merge the given array into the given Collection.
	 * @param array
	 * @param collection
	 */
	@SuppressWarnings({"unchecked", "rawtypes"})
	public static void mergeArrayIntoCollection(Object array, Collection collection) {
		Assert.notNull(collection, "Collection must not be null");
		
		collection.addAll(arrayToList(array));
	}
	
	/**
	 * Merge the given Properties instance into the given Map,
	 * copying all properties (key-value pairs) over.
	 * @param props
	 * @param map
	 */
	@SuppressWarnings({"unchecked", "rawtypes"})
	public static void mergePropertiesIntoMap(Properties props, Map map) {
		Assert.notNull(map, "Map must not be null");
		
		if (props != null) {
			Enumeration en = props.propertyNames();
			while (en.hasMoreElements()) {
				String key = (String)en.nextElement();
				Object value = props.getProperty(key);
				if (value == null) {
					value = props.get(key);
				}
				map.put(key, value);
			}
		}
	}
	
	/**
	 * Check whether the given Iterator contains the given element.
	 * @param iterator
	 * @param element
	 * @return
	 */
	@SuppressWarnings("rawtypes")
	public static boolean contains(Iterator iterator, Object element) {
		if (iterator != null) {
			while (iterator.hasNext()) {
				Object object = (Object) iterator.next();
				if (ObjectUtils.nullSafeEquals(object, element)) {
					return true;
				}
			}
		}
		return false;
	}
	
	/**
	 * Check whether the given Enumeration contains the given element.
	 * @param enumeration
	 * @param element
	 * @return
	 */
	@SuppressWarnings("rawtypes")
	public static boolean contains(Enumeration enumeration, Object element) {
		if (enumeration != null) {
			while (enumeration.hasMoreElements()) {
				Object object = (Object) enumeration.nextElement();
				if (ObjectUtils.nullSafeEquals(object, element)) {
					return true;
				}
			}
		}
		return false;
	}
	
	/**
	 * Check whether the given Collection contains the given element instance.
	 * Enforces the given instance to be present, rather than returning true
	 * for an equal element as well.
	 * @param collection
	 * @param element
	 * @return
	 */
	public static boolean containsInstance(Collection<?> collection, Object element) {
		if (isEmpty(collection)) {
			return false;
		}
		
		for (Object object : collection) {
			if (object == element) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Return true if any element in 'candidates' is contained in 'source';
	 * otherwise returns false.
	 * @param source
	 * @param candidates
	 * @return
	 */
	public static boolean containsAny(Collection<?> source, Collection<?> candidates) {
		if (isEmpty(source) || isEmpty(candidates)) {
			return false;
		}
		
		for (Object candidate : candidates) {
			if (source.contains(candidate)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Return the first element in 'candidates' that is contained in 'source'.
	 * If no element in 'candidates' is present in 'source' returns null. 
	 * @param source
	 * @param candidates
	 * @return
	 */
	public static Object findFirstMatch(Collection<?> source, Collection<?> candidates) {
		if (isEmpty(source) || isEmpty(candidates)) {
			return null;
		}
		for (Object candidate : candidates) {
			if (source.contains(candidate)) {
				return candidate;
			}
		}
		return null;
	}
	
	/**
	 * Find a single value of the given type in the given Collection.
	 * @param collection
	 * @param type
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static <T> T findValueOfType(Collection<?> collection, Class<T> type) {
		if (isEmpty(collection)) {
			return null;
		}
		if (type == null) {
			return null;
		}
		T value = null;
		for (Object object : collection) {
			if (type.isInstance(object)) {
				if (value != null) {
					return null;
				}
				value = (T) object;
			}
		}
		return value;
		
	}
	
	/**
	 * 
	 * @param collection
	 * @param types
	 * @return
	 */
	public static Object findValueOfType(Collection<?> collection, Class<?>[] types) {
		if (isEmpty(collection) || ObjectUtils.isEmpty(types)) {
			return null;
		}
		
		for (Class<?> type : types) {
			Object value = findValueOfType(collection, type);
			if (value != null) {
				return value;
			}
		}
		return null;
	}
	
	/**
	 * Determine whether the given Collection only contains a single unique object.
	 * @param collection
	 * @return
	 */
	@SuppressWarnings("rawtypes")
	public static boolean hasUniqueObject(Collection collection) {
		if (isEmpty(collection)) {
			return false;
		}
		
		boolean hasElement = false;
		Object element = null;
		for (Object obj : collection) {
			if (!hasElement) {
				hasElement = true;
				element = obj;
				continue;
			}
			if (element != obj) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * Find the common element type of the given Collection, if any.
	 * @param collection
	 * @return
	 */
	@SuppressWarnings("rawtypes")
	public static Class<?> findCommonElementType(Collection collection) {
		if (isEmpty(collection)) {
			return null;
		}
		Class<?> candidate = null;
		for (Object obj : collection) {
			if (obj != null) {
				if (candidate == null) {
					candidate = obj.getClass();
				}
				else if (candidate != obj.getClass()) {
					return null;
				}
			}
		}
		return candidate;
	}
	
	/**
	 * Adapts an enumeration to an iterator.
	 * @param enumeration
	 * @return
	 */
	public static <E> Iterator<E> toIterator(Enumeration<E> enumeration) {
		return new EnumerationIterator<E>(enumeration);
	}
	
	//------------------------------------------------------------------
	// Iterator wrapping an Enumeration.
	//------------------------------------------------------------------
	
	private static class EnumerationIterator<E> implements Iterator<E> {
		private Enumeration<E> enumeration;

		public EnumerationIterator(Enumeration<E> enumeration) {
			this.enumeration = enumeration;
		}

		@Override
		public boolean hasNext() {
			return enumeration.hasMoreElements();
		}

		@Override
		public E next() {
			return enumeration.nextElement();
		}

		@Override
		public void remove() {
			throw new UnsupportedOperationException("Not supported");
		}
		
	}
}
