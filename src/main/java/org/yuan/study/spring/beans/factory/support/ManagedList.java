package org.yuan.study.spring.beans.factory.support;

import java.util.ArrayList;
import java.util.List;

import org.yuan.study.spring.beans.BeanMetadataElement;
import org.yuan.study.spring.beans.Mergeable;

public class ManagedList<E> extends ArrayList<E> implements Mergeable, BeanMetadataElement {
	private static final long serialVersionUID = 1L;

	private Object source;
	
	private String elementTypeName;
	
	private boolean mergeEnabled;
	
	public ManagedList() {
	}

	public ManagedList(int initialCapacity) {
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
	public Object merge(Object parent) {
		if (!mergeEnabled) {
			throw new IllegalStateException("Not allowed to merge when the 'mergeEnabled' property is set to 'false'");
		}
		if (parent == null) {
			return this;
		}
		if (!(parent instanceof List)) {
			throw new IllegalArgumentException(String.format("Cannot merge with object of type [%s]", parent.getClass()));
		}
		List<E> merged = new ManagedList<E>();
		merged.addAll((List) parent);
		merged.addAll(this);
		return merged;
	}

	/**
	 * Return the default element type name to be used for this list.
	 * @return
	 */
	public String getElementTypeName() {
		return elementTypeName;
	}

	/**
	 * Set the default element type name to be used for this list.
	 * @param elementTypeName
	 */
	public void setElementTypeName(String elementTypeName) {
		this.elementTypeName = elementTypeName;
	}

	/**
	 * Set whether merging should be enabled for this collection,
	 * in case of a 'parent' collection value being present.
	 * @param mergedEnabled
	 */
	public void setMergeEnabled(boolean mergeEnabled) {
		this.mergeEnabled = mergeEnabled;
	}

	/**
	 * Set the configuration source Object for this metadata element.
	 * @param source
	 */
	public void setSource(Object source) {
		this.source = source;
	}
	
}
