package org.yuan.study.spring.beans.factory.support;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

public class ManagedMap<K, V> implements Map<K, V> {

	private final Map<K,V> map;
	
	public ManagedMap() {
		this(16);
	}
	
	public ManagedMap(int initialCapacity) {
		this.map = new LinkedHashMap<K,V>(initialCapacity);
	}

	public ManagedMap(Map<K, V> map) {
		this.map = map;
	}

	//----------------------------------------------
	// Implementation of Map interface
	//----------------------------------------------
	
	@Override
	public int size() {
		return map.size();
	}

	@Override
	public boolean isEmpty() {
		return map.isEmpty();
	}

	@Override
	public boolean containsKey(Object key) {
		return map.containsKey(key);
	}

	@Override
	public boolean containsValue(Object value) {
		return map.containsValue(value);
	}

	@Override
	public V get(Object key) {
		return map.get(key);
	}

	@Override
	public V put(K key, V value) {
		return map.put(key, value);
	}

	@Override
	public V remove(Object key) {
		return map.remove(key);
	}

	@Override
	public void putAll(Map<? extends K, ? extends V> m) {
		map.putAll(m);
	}

	@Override
	public void clear() {
		map.clear();
	}

	@Override
	public Set<K> keySet() {
		return map.keySet();
	}

	@Override
	public Collection<V> values() {
		return map.values();
	}

	@Override
	public Set<java.util.Map.Entry<K, V>> entrySet() {
		return map.entrySet();
	}

	
	//----------------------------------------------------------
	// Implementation of other methods
	//----------------------------------------------------------
	
	@Override
	public int hashCode() {
		return map.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		return map.equals(obj);
	}

	@Override
	public String toString() {
		return map.toString();
	}
}
