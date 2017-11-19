package org.yuan.study.spring.util;

import java.util.List;
import java.util.Map;

public interface MultiValueMap<K, V> extends Map<K, List<V>> {

	/**
	 * Return the first value for the given key.
	 * @param key
	 * @return
	 */
	V getFirst(K key);
	
	/**
	 * Add the given single value to the current list of values for the given key.
	 * @param key
	 * @param value
	 */
	void add(K key, V value);
	
	/**
	 * Set the given single value under the given key.
	 * @param key
	 * @param value
	 */
	void set(K key, V value);
	
	/**
	 * Set the given values under.
	 * @param values
	 */
	void setAll(Map<K, V> values);
	
	/**
	 * Returns the first values contained in this MultiValueMap.
	 * @return
	 */
	Map<K, V> toSingleValueMap();
}
