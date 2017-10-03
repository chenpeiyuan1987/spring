package org.yuan.study.spring.core;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.CopyOnWriteArraySet;

import org.yuan.study.spring.util.LinkedCaseInsensitiveMap;

/**
 * Factory for collections, being aware of Java 5 and Java 6 collections.
 * Mainly for internal use within the framworkk.
 * 
 * The goal of this class is to avoid runtime dependencies on a specific
 * Java version, while nevertheless using the best collection implementation
 * that is available at runtime.
 * 
 * @author Yuan
 *
 */
public abstract class CollectionFactory {
	
	private static Class navigableSetClass = null;
	
	private static Class navigableMapClass = null;
	
	private static final Set<Class> approximableCollectionTypes = new HashSet<Class>(10);
	
	private static final Set<Class> approximableMapTypes = new HashSet<Class>(6);

	static {
		approximableCollectionTypes.add(Collection.class);
		approximableCollectionTypes.add(List.class);
		approximableCollectionTypes.add(Set.class);
		approximableCollectionTypes.add(SortedSet.class);
		approximableMapTypes.add(Map.class);
		approximableMapTypes.add(SortedMap.class);
		
		ClassLoader cl = CollectionFactory.class.getClassLoader();
		try {
			navigableSetClass = cl.loadClass("java.util.NavigableSet");
			navigableMapClass = cl.loadClass("java.util.NavigableMap");
			approximableCollectionTypes.add(navigableSetClass);
			approximableMapTypes.add(navigableMapClass);
		}
		catch (ClassNotFoundException ex) {
			
		}
		
		approximableCollectionTypes.add(ArrayList.class);
		approximableCollectionTypes.add(LinkedList.class);
		approximableCollectionTypes.add(HashSet.class);
		approximableCollectionTypes.add(LinkedHashSet.class);
		approximableCollectionTypes.add(TreeSet.class);
		approximableMapTypes.add(HashMap.class);
		approximableMapTypes.add(LinkedHashMap.class);
		approximableMapTypes.add(TreeMap.class);
	}
	
	/**
	 * Create a linked Set if possible: This implementation always
	 * creates a java.util.LinkedHashSet, since Spring 2.5 requires
	 * JDK 1.4 anyway.
	 * @param initial
	 * @return
	 */
	@Deprecated
	public static <T> Set<T> createLinkedSetIfPossible(int initial) {
		return new LinkedHashSet<T>(initial);
	}
	
	/**
	 * Create a copy-on-write Set (allowing for synchronization-less iteration) if possible:
	 * This implementation always creates a java.util.concurrent.CopyOnWriteArraySet,
	 * since Spring 3 requires JDK 1.5 anyway.
	 * @return
	 */
	@Deprecated
	public static <T> Set<T> createCopyOnWriteSet() {
		return new CopyOnWriteArraySet<T>();
	}
	
	/**
	 * Create a linked Map if possible: This implementation always
	 * creates a java.util.LinkedHashMap, since Spring 2.5 requires
	 * JDK 1.4 anyway.
	 * @param initial
	 * @return
	 */
	@Deprecated
	public static <K,V> Map<K,V> createLinkedMapIfPossible(int initial) {
		return new LinkedHashMap<K,V>(initial);
	}
	
	/**
	 * Create a linked case-insensitive Map if possible: This implementation
	 * always returns a org.springframework.util.LinkedCaseInsensitiveMap.
	 * @param initial
	 * @return
	 */
	@Deprecated
	public static <V> Map<String,V> createLinkedCaseInsensitiveMapIfPossible(int initial) {
		return new LinkedCaseInsensitiveMap<V>(initial);
	}
	
	/**
	 * Create an identity Map if possible: This implementation always
	 * creates a java.util.IdentityHashMap, since Spring 2.5 requires 
	 * JDK 1.4 anyway.
	 * @param initial
	 * @return
	 */
	@Deprecated
	public static <K,V> Map<K,V> createIdentityMapIfPossible(int initial) {
		return new IdentityHashMap<K,V>(initial);
	}
	
	/**
	 * Create a concurrent Map if possible: This implementation always
	 * creates a java.util.concurrent.ConcurrentHashMap, since Spring 
	 * 3.0 requires JDK 1.5 anyway.
	 * @param initial
	 * @return
	 */
	public static <K,V> Map<K,V> createConcurrentMapIfPossible(int initial) {
		return new ConcurrentHashMap<K,V>(initial);
	}
	
	/**
	 * Create a concurrent Map with a dedicated ConcurrentMap interface:
	 * This implementation always creates a java.util.concurrent.ConcurrentHashMap,
	 * since Spring 3.0 requires JDK 1.5 anyway.
	 * @param initial
	 * @return
	 */
	@Deprecated
	public static <K,V> ConcurrentHashMap<K,V> createConcurrentMap(int initial) {
		return new JdkConcurrentHashMap<K,V>(initial);
	}
	
	/**
	 * Determine whether the given collection type is an approximable type,
	 * i.e. a type that can approximate.
	 * @param collectionType
	 * @return
	 */
	public static boolean isApproximableCollectionType(Class<?> collectionType) {
		return (collectionType != null && approximableCollectionTypes.contains(collectionType));
	}
	
	/**
	 * Create the most approximate collection for the given collection.
	 * Creates an ArrayList, TreeSet or linked Set for a List, SortedSet
	 * or Set, respectively.
	 * @param collection
	 * @param initial
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static <T> Collection<T> createApproximateCollection(Object collection, int initial) {
		if (collection instanceof LinkedList) {
			return new LinkedList<T>();
		}
		else if (collection instanceof List) {
			return new ArrayList<T>(initial);
		}
		else if (collection instanceof SortedSet) {
			return new TreeSet<T>(((SortedSet<T>) collection).comparator());
		}
		else {
			return new LinkedHashSet<T>(initial);
		}
	}
	
	/**
	 * Create the most appropriate collection for the given collection type.
	 * Creates an ArrayList, TreeSet or linked Set for a List, SortedSet
	 * or Set, respectively.
	 * @param collectionType
	 * @param initial
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static <T> Collection<T> createCollection(Class<?> collectionType, int initial) {
		if (collectionType.isInterface()) {
			if (List.class.equals(collectionType)) {
				return new ArrayList<T>(initial);
			} 
			else if (SortedSet.class.equals(collectionType) || collectionType.equals(navigableSetClass)) {
				return new TreeSet<T>();
			}
			else if (Set.class.equals(collectionType) || Collection.class.equals(collectionType)) {
				return new LinkedHashSet<T>(initial);
			}
			else {
				throw new IllegalArgumentException("Unsupported Collection interface: " + collectionType.getName());
			}
		} 
		else {
			if (!Collection.class.isAssignableFrom(collectionType)) {
				throw new IllegalArgumentException("Unsupported Collection type: " + collectionType.getName());
			}
			try {
				return (Collection<T>) collectionType.newInstance();
			}
			catch (Exception ex) {
				throw new IllegalArgumentException("Could not instantiate Collection type: " + collectionType.getName());
			}
		}
	}
	
	/**
	 * Determine whether the given map type is an approximable type,
	 * i.e. a type that can approximate.
	 * @param mapType
	 * @return
	 */
	public static boolean isApproximableMapType(Class<?> mapType) {
		return (mapType != null && approximableMapTypes.contains(mapType));
	}
	
	/**
	 * Create the most approximate map for the given map.
	 * Creates a TreeMap or linked Map for a SortedMap or Map,
	 * respectively.
	 * @param map
	 * @param initial
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static <K,V> Map<K,V> createApproximateMap(Object map, int initial) {
		if (map instanceof SortedMap) {
			return new TreeMap<K,V>(((SortedMap<K,V>) map).comparator());
		}
		else {
			return new LinkedHashMap<K,V>(initial);
		}
	}
	
	/**
	 * Create the msot approximate map for the given map.
	 * Creates a TreeMap or linked Map for a SortedMap or Map, 
	 * respectively.
	 * @param mapType
	 * @param initial
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static <K,V> Map<K,V> createMap(Class<?> mapType, int initial) {
		if (mapType.isInterface()) {
			if (Map.class.equals(mapType)) {
				return new LinkedHashMap<K,V>(initial);
			} 
			else if (SortedMap.class.equals(mapType) || mapType.equals(navigableMapClass)) {
				return new TreeMap<K,V>();
			}
			else {
				throw new IllegalArgumentException("Unsupported Map interface: " + mapType.getName());
			}
		} 
		else {
			if (!Map.class.isAssignableFrom(mapType)) {
				throw new IllegalArgumentException("Unsupported Map type: " + mapType.getName());
			}
			try {
				return (Map<K,V>) mapType.newInstance();
			}
			catch (Exception ex) {
				throw new IllegalArgumentException("Could not instantiate Map type: " + mapType.getName());
			}
		}
	}
	
	/**
	 * ConcurrentMap adapter for the JDK ConcurrentHashMap class.
	 */
	@Deprecated
	private static class JdkConcurrentHashMap<K,V> extends ConcurrentHashMap<K,V> implements ConcurrentMap<K,V> {
		private JdkConcurrentHashMap(int initial) {
			super(initial);
		}
	}
}
