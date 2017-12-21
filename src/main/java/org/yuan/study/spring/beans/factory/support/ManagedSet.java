package org.yuan.study.spring.beans.factory.support;

import java.util.LinkedHashSet;
import java.util.Set;

import org.yuan.study.spring.beans.BeanMetadataElement;
import org.yuan.study.spring.beans.Mergeable;


public class ManagedSet<E> extends LinkedHashSet<E> implements Mergeable, BeanMetadataElement {
	private static final long serialVersionUID = 1L;

	private Object source;
	
	private String elementTypeName;
	
	private boolean mergeEnabled;
	
	public ManagedSet() {
	}
	
	public ManagedSet(int initialCapacity) {
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
	public Set<E> merge(Object parent) {
		if (!mergeEnabled) {
			throw new IllegalStateException("Not allowed to merge when the 'mergeEnabled' property is set to 'false'");
		}
		if (parent == null) {
			return this;
		}
		if (!(parent instanceof Set)) {
			throw new IllegalArgumentException(String.format("Cannot merge with object of type [%s]", parent.getClass()));
		}
		Set<E> merged = new ManagedSet<E>();
		merged.addAll((Set<E>) parent);
		merged.addAll(this);
		return merged;
	}

	/**
	 * Set the default element type name to be used for this set.
	 * @return
	 */
	public String getElementTypeName() {
		return elementTypeName;
	}

	/**
	 * Return the default element type name to be used for this set.
	 * @param elementTypeName
	 */
	public void setElementTypeName(String elementTypeName) {
		this.elementTypeName = elementTypeName;
	}

	/**
	 * Set the configuration source Object for this metadata element.
	 * @param source
	 */
	public void setSource(Object source) {
		this.source = source;
	}

	/**
	 * Set whether merging should be enabled for this collection.
	 * @param mergeEnabled
	 */
	public void setMergeEnabled(boolean mergeEnabled) {
		this.mergeEnabled = mergeEnabled;
	}

}
