package org.yuan.study.spring.beans.factory.config;

import org.yuan.study.spring.beans.factory.ObjectFactory;

public interface Scope {

	/**
	 * Return the object with the given name from the underlying scope,
	 * if not found in the underlying storage mechanism.
	 * @param name
	 * @param objectFactory
	 * @return
	 */
	Object get(String name, ObjectFactory<?> objectFactory);
	
	/**
	 * Remove the object with the given 'name' from the underlying scope.
	 * @param name
	 * @return
	 */
	Object remove(String name);
	
	/**
	 * Register a callback to be executed on destruction of the specified.
	 * @param name
	 * @param callback
	 */
	void registerDestructionCallback(String name, Runnable callback);
	
	/**
	 * Resolve the contextual object for the given key, if any.
	 * @param key
	 * @return
	 */
	Object resolveContextualObject(String key);
	
	/**
	 * Return the 'conversation id' for the current underlying scope, if any.
	 * @return
	 */
	String getConversationId();
}
