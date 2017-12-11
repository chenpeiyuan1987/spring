package org.yuan.study.spring.core;

public interface AliasRegistry {
	
	/**
	 * Given a name, register an alias for it.
	 * @param name
	 * @param alias
	 */
	void registerAlias(String name, String alias);
	
	/**
	 * Remove the specified alias from this registry.
	 * @param alias
	 */
	void removeAlias(String alias);
	
	/**
	 * Determine whether this given name is defines as an alias.
	 * @param name
	 * @return
	 */
	boolean isAlias(String name);
	
	/**
	 * Return the aliases for the given name, if defined.
	 * @param name
	 * @return
	 */
	String[] getAliases(String name);
}
