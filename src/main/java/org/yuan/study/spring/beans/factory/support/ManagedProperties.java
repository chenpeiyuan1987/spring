package org.yuan.study.spring.beans.factory.support;

import java.util.Properties;

import org.yuan.study.spring.beans.BeanMetadataElement;
import org.yuan.study.spring.beans.Mergeable;

public class ManagedProperties extends Properties implements Mergeable, BeanMetadataElement {
	private static final long serialVersionUID = 1L;

	private Object source;
	
	private boolean mergeEnabled;

	public Object getSource() {
		return source;
	}

	/**
	 * Set the configuration source Object for this metadata element.
	 * @param source
	 */
	public void setSource(Object source) {
		this.source = source;
	}

	@Override
	public boolean isMergeEnabled() {
		return mergeEnabled;
	}

	/**
	 * Set whether merging should be enabbled for this collection,
	 * in case of a ''parent' collection value being present.
	 * @param mergeEnabled
	 */
	public void setMergeEnabled(boolean mergeEnabled) {
		this.mergeEnabled = mergeEnabled;
	}

	@Override
	public Object merge(Object parent) {
		if (!mergeEnabled) {
			throw new IllegalStateException("Not allowed to merge when the 'mergeEnabled' property is set to 'false'");
		}
		if (parent == null) {
			return this;
		}
		if (!(parent instanceof Properties)) {
			throw new IllegalArgumentException(String.format("Cannot merge with object of type [%s]", parent.getClass()));
		}
		Properties merged = new ManagedProperties();
		merged.putAll((Properties) parent);
		merged.putAll(this);
		return merged;
	}
	
}
