package org.yuan.study.spring.beans;

public abstract class PropertyAccessorUtils {

	/**
	 * Return the actual property name for the given property path.
	 * @param propertyPath
	 * @return
	 */
	public static String getPropertyName(String propertyPath) {
		int separatorIndex = propertyPath.indexOf(PropertyAccessor.PROPERTY_KEY_PREFIX_CHAR);
		return (separatorIndex != -1 ? propertyPath.substring(0, separatorIndex) : propertyPath);
	}
	
	/**
	 * Check whether the given property path indicates an indexed or nested property.
	 * @param propertyPath
	 * @return
	 */
	public static boolean isNestedOrIndexedProperty(String propertyPath) {
		if (propertyPath == null) {
			return false;
		}
		for (int i = 0; i < propertyPath.length(); i++) {
			char ch = propertyPath.charAt(i);
			if (ch == PropertyAccessor.NESTED_PROPERTY_SEPARATOR_CHAR
				|| ch == PropertyAccessor.PROPERTY_KEY_PREFIX_CHAR) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Determine the first nested property separator in the given property path, ignoring dots in keys.
	 * @param propertyPath
	 * @return
	 */
	public static int getFirstNestedPropertySeparatorIndex(String propertyPath) {
		return getNestedPropertySeparatorIndex(propertyPath, false);
	}
	
	/**
	 * Determine the first nested property separator in the given property path, ignoring dots in keys.
	 * @param propertyPath
	 * @return
	 */
	public static int getLastNestedPropertySeparatorIndex(String propertyPath) {
		return getNestedPropertySeparatorIndex(propertyPath, true);
	}
	
	/**
	 * Determine the first nested property separator in the given property path, ignoring dots in keys.
	 * @param propertyPath
	 * @param last
	 * @return
	 */
	private static int getNestedPropertySeparatorIndex(String propertyPath, boolean last) {
		boolean inKey = false;
		final int length = propertyPath.length();
		int i = (last ? length - 1 : 0);
		while (last ? i >= 0 : i < length) {
			switch (propertyPath.charAt(i)) {
			case PropertyAccessor.PROPERTY_KEY_PREFIX_CHAR:
			case PropertyAccessor.PROPERTY_KEY_SUFFIX_CHAR:
				inKey = !inKey;
				break;
			case PropertyAccessor.NESTED_PROPERTY_SEPARATOR_CHAR:
				if (!inKey) {
					return i;
				}
			}
			if (last) {
				i--;
			}
			else {
				i++;
			}
		}
		return -1;
	}
	
	/**
	 * Determine the canonical name for the given property path.
	 * @param propertyName
	 * @return
	 */
	public static String canonicalPropertyName(String propertyName) {
		if (propertyName == null) {
			return "";
		}
		
		StringBuffer sb = new StringBuffer(propertyName);
		int indexStart = 0;
		while (indexStart != -1) {
			indexStart = sb.indexOf(PropertyAccessor.PROPERTY_KEY_PREFIX, indexStart);
			if (indexStart != -1) {
				int indexFinis = sb.indexOf(PropertyAccessor.PROPERTY_KEY_SUFFIX, indexStart + PropertyAccessor.PROPERTY_KEY_PREFIX.length());
				if (indexFinis != -1) {
					int indexStart2 = indexStart + PropertyAccessor.PROPERTY_KEY_PREFIX.length();
					String key = sb.substring(indexStart2, indexFinis);
					if ((key.startsWith("'") && key.endsWith("'")) || (key.startsWith("\"") && key.endsWith("\""))) {
						sb.delete(indexFinis - 1, indexFinis);
						sb.delete(indexStart2, indexStart2 + 1);
						indexFinis = indexFinis - 2;
					}
					indexStart = indexFinis + PropertyAccessor.PROPERTY_KEY_SUFFIX.length();
				}
			}
		}
		return sb.toString();
	}
	
	/**
	 * Determine the canonical names for the given property paths.
	 * @param propertyNames
	 * @return
	 */
	public static String[] canonicalPropertyNames(String[] propertyNames) {
		if (propertyNames == null) {
			return null;
		}
		String[] result = new String[propertyNames.length];
		for (int i = 0; i < result.length; i++) {
			result[i] = canonicalPropertyName(propertyNames[i]);
		}
		return result;
	}
	
	/**
	 * Determine whether the given registered path matches the given property path,
	 * either indicating the property itself or an indexed element of the property.
	 * @param str
	 * @param path
	 * @return
	 */
	public static boolean matchesProperty(String registeredPath, String propertyPath) {
		if (!registeredPath.startsWith(propertyPath)) {
			return false;
		}
		if (registeredPath.length() == propertyPath.length()) {
			return true;
		}
		if (registeredPath.charAt(propertyPath.length()) != PropertyAccessor.PROPERTY_KEY_PREFIX_CHAR) {
			return false;
		}
		if (registeredPath.charAt(registeredPath.length() - 1) == PropertyAccessor.PROPERTY_KEY_SUFFIX_CHAR) {
			return true;
		}
		return false;
	}
}
