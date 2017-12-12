package org.yuan.study.spring.core;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.StringUtils;
import org.yuan.study.spring.util.StringValueResolver;

public class SimpleAliasRegistry implements AliasRegistry {
	
	/** Map from alias to canonical name */
	private final Map<String, String> aliasMap = new ConcurrentHashMap<String, String>();
	
	//----------------------------------------------------------------------------------
	// Implementation of AliasRegistry Methods
	//----------------------------------------------------------------------------------

	@Override
	public void registerAlias(String name, String alias) {
		Assert.hasText(name, "Name must not be empty");
		Assert.hasText(alias, "Alias must not be empty");
		
		if (alias.equals(name)) {
			aliasMap.remove(alias);
		} 
		else {
			if (!allowAliasOverriding()) {
				String temp = aliasMap.get(alias);
				if (temp != null && !temp.equals(name)) {
					throw new IllegalStateException(String.format(
						"Cannot register alias '%s' for name '%s': It is already registered for name '%s'.", alias, name, temp));
				}
			}
			checkForAliasCircle(name, alias);
			aliasMap.put(alias, name);
		}
	}

	@Override
	public void removeAlias(String alias) {
		String name = aliasMap.remove(alias);
		if (name == null) {
			throw new IllegalStateException(
				String.format("No alias '%s' registered", alias));
		}
	}

	@Override
	public boolean isAlias(String name) {
		return aliasMap.containsKey(name);
	}

	@Override
	public String[] getAliases(String name) {
		List<String> result = new ArrayList<String>();
		synchronized (aliasMap) {
			retrieveAliases(name, result);
		}
		return StringUtils.toStringArray(result);
	}

	//---------------------------------------------------------------------------------
	// Implementation of Methods
	//---------------------------------------------------------------------------------
	
	/**
	 * Transitively retrieve all aliases for the given name.
	 * @param valueResolver
	 */
	public void resolveAliases(StringValueResolver valueResolver) {
		Assert.notNull(valueResolver, "StringValueResolver must not be null");
		
		synchronized (aliasMap) {
			Map<String, String> aliasCopy = new HashMap<String, String>(aliasMap);
			for (String alias : aliasCopy.keySet()) {
				String registeredName = aliasCopy.get(alias);
				String resolvedAlias = valueResolver.resolveStringValue(alias);
				String resolvedName = valueResolver.resolveStringValue(registeredName);
				if (resolvedAlias.equals(resolvedName)) {
					aliasMap.remove(alias);
				}
				else if (!resolvedAlias.equals(alias)) {
					String existingName = aliasMap.get(resolvedAlias);
					if (existingName != null && !existingName.equals(resolvedName)) {
						throw new IllegalStateException(String.format(
							"Cannot register resolved alias '%s' (original: '%s') for name '%s': It is already registered for name '%s'.", 
								resolvedAlias, alias, resolvedName, registeredName));
					}
					checkForAliasCircle(resolvedName, resolvedAlias);
					aliasMap.remove(alias);
					aliasMap.put(resolvedAlias, registeredName);
				}
				else if (!registeredName.equals(resolvedName)) {
					aliasMap.put(alias, resolvedName);
				}
			}
		}
	}
	
	/**
	 * Determine the raw name, resolving aliases to canonical names.
	 * @param name
	 * @return
	 */
	public String canonicalName(String name) {
		while (true) {
			String temp = aliasMap.get(name);
			if (temp != null) {
				name = temp;
			} 
			else {
				return name;
			}
		}
	}
	
	/**
	 * Return whether alias overriding is allowed.
	 * @return
	 */
	protected boolean allowAliasOverriding() {
		return true;
	}
	
	/**
	 * Transitively retrieve all aliases for the given name.
	 * @param name
	 * @param result
	 */
	private void retrieveAliases(String name, List<String> result) {
		for (Entry<String, String> entry : aliasMap.entrySet()) {
			String temp = entry.getValue();
			if (temp.equals(name)) {
				String alias = entry.getKey();
				result.add(alias);
				retrieveAliases(alias, result);
			}
		}
	}
	
	/**
	 * Check whether the given name points back to given alias as an alias
	 * in the other direction, catching a circular reference upfront and throwing
	 * a corresponding IllegalStateException.
	 * @param name
	 * @param alias
	 */
	protected void checkForAliasCircle(String name, String alias) {
		if (alias.equals(canonicalName(name))) {
			throw new IllegalStateException(String.format(
				"Cannot register alias '%s' for name '%s': Circular reference - "
				+ "'%s' is a direct or indirect alias for '%s' already", 
					alias, name, name, alias));
		}
	}
}
