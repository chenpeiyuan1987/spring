package org.yuan.study.spring.beans.factory.support;

import java.util.LinkedHashMap;
import java.util.Map;

import org.yuan.study.spring.beans.BeanMetadataElement;
import org.yuan.study.spring.beans.Mergeable;

public class ManagedMap<K, V> extends LinkedHashMap<K, V> implements Mergeable, BeanMetadataElement {
	private static final long serialVersionUID = 1L;

	private Object source;
	
	private String keyTypeName;
	
	private String valueTypeName;
	
	private boolean mergeEnabled;
	
	public ManagedMap() {
	}
	
	public ManagedMap(int initialCapacity) {
		super(initialCapacity);
	}

	@Override
	public Object getSource() {
		return source;
	}

	@Override
	public boolean isMergeEnabled() {
		return mergeEnabled;
	}

	@Override
	@SuppressWarnings("unchecked")
	public Object merge(Object parent) {
		if (!mergeEnabled) {
			throw new IllegalStateException("Not allowed to merge when the 'mergeEnabled' property is set to 'false'");
		}
		if (parent == null) {
			return this;
		}
		if (!(parent instanceof Map)) {
			throw new IllegalArgumentException(String.format("Cannot merge with object of type [%s]", parent.getClass()));
		}
		Map<K, V> merged = new ManagedMap<K, V>();
		merged.putAll((Map<K, V>) parent);
		merged.putAll(this);
		return merged;
	}

	/**
	 * Return the default key type name to be used for this map.
	 * @return
	 */
	public String getKeyTypeName() {
		return keyTypeName;
	}

	/**
	 * Set the default key type name to be used for this map.
	 * @param keyTypeName
	 */
	public void setKeyTypeName(String keyTypeName) {
		this.keyTypeName = keyTypeName;
	}

	/**
	 * Return the default value type name to be used for this map.
	 * @return
	 */
	public String getValueTypeName() {
		return valueTypeName;
	}

	/**
	 * Set the default value type name to be used for this map.
	 * @param valueTypeName
	 */
	public void setValueTypeName(String valueTypeName) {
		this.valueTypeName = valueTypeName;
	}

	/**
	 * Set the configuration source Object for this metadata element.
	 * @param source
	 */
	public void setSource(Object source) {
		this.source = source;
	}

	/**
	 * Set whether merging should be enabled for this collection,
	 * in case of a 'parent' collection value being present.
	 * @param mergeEnabled
	 */
	public void setMergeEnabled(boolean mergeEnabled) {
		this.mergeEnabled = mergeEnabled;
	}
	
}
