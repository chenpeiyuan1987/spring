package org.yuan.study.spring.beans;

public interface Mergeable {

	/**
	 * Is merging enabled for this particular instance?
	 */
	boolean isMergeEnabled();
	
	/**
	 * Merge the current value set with that of the supplied object.
	 */
	Object merge(Object parent);
	
}
