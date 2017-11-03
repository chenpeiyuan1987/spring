package org.yuan.study.spring.util;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class PropertyPlaceholderHelper {
	
	private static final Log log = LogFactory.getLog(PropertyPlaceholderHelper.class);
	
	private static final Map<String, String> wellKnownSimplePrefixes = new HashMap<String, String>(4);
	
	static {
		wellKnownSimplePrefixes.put("}", "{");
		wellKnownSimplePrefixes.put("]", "[");
		wellKnownSimplePrefixes.put(")", "(");
	}
	
	private final String placeholderPrefix;
	
	private final String placeholderSuffix;
	
	private final String simplePrefix;
	
	private final String valueSeparator;
	
	private final boolean ignoreUnresolvablePlaceholders;

	/**
	 * 
	 * @param placeholderPrefix
	 * @param placeholderSuffix
	 */
	public PropertyPlaceholderHelper(String placeholderPrefix, String placeholderSuffix) {
		this(placeholderPrefix, placeholderSuffix, null, true);
	}
	
	/**
	 * 
	 * @param placeholderPrefix
	 * @param placeholderSuffix
	 * @param simplePrefix
	 * @param valueSeparator
	 * @param ignoreUnresolvablePlaceholders
	 */
	public PropertyPlaceholderHelper(String placeholderPrefix, String placeholderSuffix,
		String valueSeparator, boolean ignoreUnresolvablePlaceholders) {
		
		Assert.notNull(placeholderPrefix, "placeholderPrefix must not be null");
		Assert.notNull(placeholderSuffix, "placeholderSuffix must not be null");
		this.placeholderPrefix = placeholderPrefix;
		this.placeholderSuffix = placeholderSuffix;
		String simplePrefixForSuffix = wellKnownSimplePrefixes.get(this.placeholderSuffix);
		if (simplePrefixForSuffix != null && this.placeholderPrefix.endsWith(simplePrefixForSuffix)) {
			this.simplePrefix = simplePrefixForSuffix;
		} 
		else {
			this.simplePrefix = this.placeholderPrefix;
		}
		this.valueSeparator = valueSeparator;
		this.ignoreUnresolvablePlaceholders = ignoreUnresolvablePlaceholders;
	}

	/**
	 * 
	 * @param value
	 * @param properties
	 * @return
	 */
	public String replacePlaceholders(String value, final Properties properties) {
		Assert.notNull(properties, "Argument 'properties' must not be null.");
		return replacePlaceholders(value, new PlaceholderResolver() {
			@Override
			public String resolvePlaceholder(String placeholderName) {
				return properties.getProperty(placeholderName);
			}
		});
	}
	
	/**
	 * 
	 * @param value
	 * @param placeholderResolver
	 * @return
	 */
	public String replacePlaceholders(String value, PlaceholderResolver placeholderResolver) {
		Assert.notNull(value, "Argument 'value' must not be null.");
		return parseStringValue(value, placeholderResolver, new HashSet<String>());
	}
	
	protected String parseStringValue(String strVal, PlaceholderResolver placeholderResolver, Set<String> visitedPlaceholders) {
		
	}

	private int findPlaceholderEndIndex(CharSequence buf, int startIndex) {
		
	}
	
	/**
	 * Strategy interface used to resolve replacement values for placeholders contained in Strings.
	 */
	public static interface PlaceholderResolver {
		
		/**
		 * Resolves the supplied placeholder name into the replacement value.
		 * @param placeholderName
		 * @return
		 */
		String resolvePlaceholder(String placeholderName);
	}
}
