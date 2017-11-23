package org.yuan.study.spring.core;

public interface AttributeAccessor {

	/**
	 * Set the attribute defined by name to the supplied value.
	 * @param name
	 * @param value
	 */
	void setAttribute(String name, Object value);
	
	/**
	 * Get the value of the attribute identified by name.
	 * @param name
	 * @return
	 */
	Object getAttribute(String name);
	
	/**
	 * Remove the attribute identified by name and return its name.
	 * @param name
	 * @return
	 */
	Object removeAttribute(String name);
	
	/**
	 * Return true if the attribute identified by name exists.
	 * @param name
	 * @return
	 */
	boolean hasAttribute(String name);
	
	/**
	 * Return the names of all attributes.
	 * @return
	 */
	String[] attributeNames();
}
