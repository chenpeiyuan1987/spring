package org.yuan.study.spring.core;

import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.collections.map.CaseInsensitiveMap;
import org.apache.commons.collections.map.IdentityMap;
import org.apache.commons.collections.map.LinkedMap;
import org.apache.commons.collections.map.ListOrderedMap;
import org.apache.commons.collections.set.ListOrderedSet;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public abstract class CollectionFactory {

	private static final String COMMONS_COLLECTIONS_CLASS_NAME = 
		"org.apache.commons.collections.map.LinkedMap";
	
	private static final Log logger = LogFactory.getLog(CollectionFactory.class);
	
	private static boolean commonsCollections3xAvailable;
	
	static {
		if (JdkVersion.getMajorJavaVersion() >= JdkVersion.JAVA_14) {
			logger.info("JDK 1.4+ collections available");
		}
		try {
			Class.forName(COMMONS_COLLECTIONS_CLASS_NAME);
			commonsCollections3xAvailable = true;
			logger.info("Commons Collections 3.x available");
		}
		catch (ClassNotFoundException ex) {
			commonsCollections3xAvailable = false;
		}
	}
	
	/**
	 * Create a linked set if possible: that is, if running on JDK >= 1.4
	 * or if Commons Collections 3.x is available.
	 * @param initialCapacity
	 * @return
	 */
	public static Set<?> createLinkedSetIfPossible(int initial) {
		if (JdkVersion.getMajorJavaVersion() >= JdkVersion.JAVA_14) {
			logger.debug("Creating [java.util.LinkedHashSet]");
			return Jdk14CollectionFactory.createLinkedHashSet(initial);
		} else if (commonsCollections3xAvailable) {
			logger.debug("Creating [org.apache.commons.collections.set.ListOrderedSet]");
			return CommonsCollectionFactory.createListOrderedSet(initial);
		}
		else {
			logger.debug("Falling back to [java.util.HashSet] for linked set");
			return new HashSet(initial);
		}
	}
	
	/**
	 * Create a linked map if possible: that is, if running on JDK >= 1.4
	 * or if Commons Collections 3.x is available.
	 * @param initialCapacity
	 * @return
	 */
	public static Map<?,?> createLinkedMaoIfPossible(int initial) {
		if (JdkVersion.getMajorJavaVersion() >= JdkVersion.JAVA_14) {
			logger.debug("Creating [java.util.LinkedHashMap]");
			return Jdk14CollectionFactory.createLinkedHashMap(initial);
		} else if (commonsCollections3xAvailable) {
			logger.debug("Creating [org.apache.commons.collections.map.LinkedMap]");
			return CommonsCollectionFactory.createLinkedMap(initial);
		}
		else {
			logger.debug("Falling back to [java.util.HashMap] for linked map");
			return new HashMap(initial);
		}
	}
	
	/**
	 * Create a linked case-insensitive map if possible.
	 * @param initialCapacity
	 * @return
	 */
	public static Map<?,?> createLinkedCaseInsensitiveMapIfPossible(int initial) {
		if (JdkVersion.getMajorJavaVersion() >= JdkVersion.JAVA_14) {
			logger.debug("Creating [java.util.LinkedHashMap]");
			return Jdk14CollectionFactory.createLinkedHashMap(initial);
		} else if (commonsCollections3xAvailable) {
			logger.debug("Creating [org.apache.commons.collections.map.LinkedMap]");
			return CommonsCollectionFactory.createListOrderedCaseInsensitiveMap(initial);
		}
		else {
			logger.debug("Falling back to [java.util.HashMap] for linked case-insensitive map");
			return new HashMap(initial);
		}
	}
	
	/**
	 * Create ab identity map if possible.
	 * @param initialCapacity
	 * @return
	 */
	public static Map<?,?> createIdentityMapIfPossible(int initial) {
		if (JdkVersion.getMajorJavaVersion() >= JdkVersion.JAVA_14) {
			logger.debug("Creating [java.util.IdentityHashMap]");
			return Jdk14CollectionFactory.createIdentityHashMap(initial);
		} else if (commonsCollections3xAvailable) {
			logger.debug("Creating [org.apache.commons.collections.map.IdentityMap]");
			return CommonsCollectionFactory.createIdentityMap(initial);
		}
		else {
			logger.debug("Falling back to [java.util.HashMap] for identity map");
			return new HashMap(initial);
		}
	}
	
	/**
	 * Actual creation of JDK 1.4+ Collections.
	 */
	private static abstract class Jdk14CollectionFactory {
		
		private static Set createLinkedHashSet(int initial) {
			return new LinkedHashSet(initial);
		}
		
		private static Map createLinkedHashMap(int initial) {
			return new LinkedHashMap(initial);
		}
		
		private static Map createIdentityHashMap(int initial) {
			return new IdentityHashMap(initial);
		}
		
	}
	
	/**
	 * Actual creation of Commons Collections.
	 */
	private static abstract class CommonsCollectionFactory {
		
		private static Set createListOrderedSet(int initial) {
			return ListOrderedSet.decorate(new HashSet(initial));
		}
		
		private static Map createLinkedMap(int initial) {
			return new LinkedMap(initial == 0 ? 1 : initial);
		}
		
		private static Map createListOrderedCaseInsensitiveMap(int initial) {
			return ListOrderedMap.decorate(new CaseInsensitiveMap(initial == 0 ? 1 : initial));
		}
		
		private static Map createIdentityMap(int initial) {
			return new IdentityMap(initial == 0 ? 1 : initial);
		}
		
	}
}
