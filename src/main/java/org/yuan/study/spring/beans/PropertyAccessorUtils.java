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
}
