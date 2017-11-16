package org.yuan.study.spring.util;

import java.util.LinkedHashMap;
import java.util.Locale;

/**
 * variant that stores String keys in a case-insensitive
 * manner, for example for key-based access in a results table.
 * 
 * Preserves the original order as well as the original casing of keys,
 * while allowing for contains, get and remove calls with any case of key.
 * 
 * Does not support null keys.
 * 
 * @author Yuan
 */
public class LinkedCaseInsensitiveMap<V> extends LinkedHashMap<String, V> {
	private static final long serialVersionUID = 1L;

	private final Locale locale;

	/**
	 * Create a new LinkedCaseInsensitiveMap for the default Locale.
	 */
	public LinkedCaseInsensitiveMap() {
		this(null);
	}

	/**
	 * Create a new LinkedCaseInsensitiveMap that stores lower-case keys
	 * according to the given Locale.
	 * @param locale
	 */
	public LinkedCaseInsensitiveMap(Locale locale) {
		super();
		this.locale = (locale != null ? locale : Locale.getDefault());
	}
	
	/**
	 * Create a new LinkedCaseInsensitiveMap that wraps a LinkedHashMap
	 * with the given initial capacity and stores lower-case keys according
	 * to the default Locale.
	 * @param initial
	 */
	public LinkedCaseInsensitiveMap(int initial) {
		this(initial, null);
	}
	
	/**
	 * Create a new LinkedCaseInsensitiveMap that wraps a LinkedHashMap
	 * with the given initial capacity and stores lower-case keys according
	 * to the given Locale.
	 * @param initial
	 * @param locale
	 */
	public LinkedCaseInsensitiveMap(int initial, Locale locale) {
		super(initial);
		this.locale = (locale != null ? locale : Locale.getDefault());
	}

	@Override
	public V get(Object key) {
		if (key instanceof String) {
			return super.get(convertKey((String) key));
		} 
		else {
			return null;
		}
	}

	@Override
	public void clear() {
		super.clear();
	}

	@Override
	public boolean containsKey(Object key) {
		return (key instanceof String && this.containsKey(key));
	}

	@Override
	public V put(String key, V value) {
		return super.put(convertKey(key), value);
	}

	@Override
	public V remove(Object key) {
		if (key instanceof String) {
			return super.remove(convertKey((String)key));
		} 
		else {
			return null;
		}
	}
	
	/**
	 * Convert the given key to case-insensitive key.
	 * The default implementation converts the key to 
	 * lower-case according to this Map's Locale.
	 * @param key
	 * @return
	 */
	protected String convertKey(String key) {
		return key.toLowerCase(this.locale);
	}
}
