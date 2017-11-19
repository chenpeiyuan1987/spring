package org.yuan.study.spring.util;

import java.io.Serializable;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class LinkedMultiValueMap<K, V> implements MultiValueMap<K, V>, Serializable {
	private static final long serialVersionUID = 1L;

	private final Map<K, List<V>> targetMap;
	
	/**
	 * Create a new LinkedMultiValueMap that wraps a LinkedHashMap.
	 */
	public LinkedMultiValueMap() {
		targetMap = new LinkedHashMap<K, List<V>>();
	}
	
	/**
	 * Create a new LinkedMultiValueMap that wraps a LinkedHashMap
	 * with the given initial capacity.
	 * @param init
	 */
	public LinkedMultiValueMap(int initial) {
		targetMap = new LinkedHashMap<K, List<V>>(initial);
	}
	
	/**
	 * Create a new LinkedMultiValueMap with the same mappings
	 * as the specified Map.
	 * @param otherMap
	 */
	public LinkedMultiValueMap(Map<K, List<V>> otherMap) {
		targetMap = new LinkedHashMap<K, List<V>>(otherMap);
	}

	@Override
	public int size() {
		return targetMap.size();
	}

	@Override
	public boolean isEmpty() {
		return targetMap.isEmpty();
	}

	@Override
	public boolean containsKey(Object key) {
		return targetMap.containsKey(key);
	}

	@Override
	public boolean containsValue(Object value) {
		return targetMap.containsValue(value);
	}

	@Override
	public List<V> get(Object key) {
		return targetMap.get(key);
	}

	@Override
	public List<V> put(K key, List<V> value) {
		return targetMap.put(key, value);
	}

	@Override
	public List<V> remove(Object key) {
		return targetMap.remove(key);
	}

	@Override
	public void putAll(Map<? extends K, ? extends List<V>> m) {
		targetMap.putAll(m);
	}

	@Override
	public void clear() {
		targetMap.clear();
	}

	@Override
	public Set<K> keySet() {
		return targetMap.keySet();
	}

	@Override
	public Collection<List<V>> values() {
		return targetMap.values();
	}

	@Override
	public Set<Entry<K, List<V>>> entrySet() {
		return targetMap.entrySet();
	}

	@Override
	public V getFirst(K key) {
		List<V> values = targetMap.get(key);
		return (values != null ? values.get(0) : null);
	}

	@Override
	public void add(K key, V value) {
		List<V> values = targetMap.get(key);
		if (values == null) {
			values = new LinkedList<V>();
			targetMap.put(key, values);
		}
		values.add(value);
	}

	@Override
	public void set(K key, V value) {
		List<V> values = new LinkedList<V>();
		values.add(value);
		targetMap.put(key, values);
	}

	@Override
	public void setAll(Map<K, V> values) {
		for (Entry<K, V> entry : values.entrySet()) {
			set(entry.getKey(), entry.getValue());
		}
	}

	@Override
	public Map<K, V> toSingleValueMap() {
		LinkedHashMap<K, V> singleValueMap = new LinkedHashMap<K, V>(targetMap.size());
		for (Entry<K, List<V>> entry : targetMap.entrySet()) {
			singleValueMap.put(entry.getKey(), entry.getValue().get(0));
		}
		return singleValueMap;
	}

	@Override
	public int hashCode() {
		return targetMap.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		return targetMap.equals(obj);
	}

	@Override
	public String toString() {
		return targetMap.toString();
	}
	
}
