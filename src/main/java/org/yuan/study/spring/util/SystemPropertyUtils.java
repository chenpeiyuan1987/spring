package org.yuan.study.spring.util;

import org.yuan.study.spring.util.PropertyPlaceholderHelper.PlaceholderResolver;

public abstract class SystemPropertyUtils {

	/** Prefix for system property placeholders: "${" */
	public static final String PLACEHOLDER_PREFIX = "${";
	
	/** Suffix for system property placeholders: "}" */
	public static final String PLACEHOLDER_SUFFIX = "}";
	
	/** Value separator for system property placeholders: ":" */
	public static final String VALUE_SEPARATOR = ":";
	
	private static final PropertyPlaceholderHelper strictHelpler = 
		new PropertyPlaceholderHelper(PLACEHOLDER_PREFIX, PLACEHOLDER_SUFFIX, VALUE_SEPARATOR, false);
	
	private static final PropertyPlaceholderHelper nonStrictHelpler = 
		new PropertyPlaceholderHelper(PLACEHOLDER_PREFIX, PLACEHOLDER_SUFFIX, VALUE_SEPARATOR, true);
	
	/**
	 * Resolve ${...} placeholders in the given text, replacing them with corresponding system property values.
	 * @param text
	 * @return
	 */
	public static String resolvePlaceholders(String text) {
		return resolvePlaceholders(text, false);
	}
	
	/**
	 * Resolve ${...} placeholders in the given text, replacing them with corresponding system property values.
	 * Unresolvable placeholders with no default value are ignored and passed through unchanged if the 
	 * flag is set to true.
	 * @param text
	 * @param ignoreUnresolvablePlaceholders
	 * @return
	 */
	public static String resolvePlaceholders(String text, boolean ignoreUnresolvablePlaceholders) {
		PropertyPlaceholderHelper helper = (ignoreUnresolvablePlaceholders ? nonStrictHelpler : strictHelpler);
		return helper.replacePlaceholders(text, new SystemPropertyPlaceholderResolver(text));
	}
	
	private static class SystemPropertyPlaceholderResolver implements PlaceholderResolver {
		
		private final String text;

		public SystemPropertyPlaceholderResolver(String text) {
			this.text = text;
		}

		@Override
		public String resolvePlaceholder(String placeholderName) {
			try {
				String value = System.getProperty(placeholderName);
				if (value == null) {
					value = System.getenv(placeholderName);
				}
				return value;
			}
			catch (Throwable ex) {
				System.err.println(String.format(
					"Could not resolve placeholder '%s' in [%s] as system property: %s", 
						placeholderName, this.text, ex));
				return null;
			}
		}
	}
}
