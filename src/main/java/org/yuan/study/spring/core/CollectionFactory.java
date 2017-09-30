package org.yuan.study.spring.core;

import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

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
		return new HashSet(initial);
	}
	
	/**
	 * Create a linked map if possible: that is, if running on JDK >= 1.4
	 * or if Commons Collections 3.x is available.
	 * @param initialCapacity
	 * @return
	 */
	public static Map<?,?> createLinkedMaoIfPossible(int initial) {
		return new HashMap(initial);
	}
	
	/**
	 * Create a linked case-insensitive map if possible.
	 * @param initialCapacity
	 * @return
	 */
	public static Map<?,?> createLinkedCaseInsensitiveMapIfPossible(int initial) {
		return new HashMap();
	}
	
	/**
	 * Create ab identity map if possible.
	 * @param initialCapacity
	 * @return
	 */
	public static Map<?,?> createIdentityMapIfPossible(int initial) {
		return new HashMap(initial);
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
	
}
