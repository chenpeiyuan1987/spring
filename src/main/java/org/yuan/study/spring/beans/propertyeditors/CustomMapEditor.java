package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import org.yuan.study.spring.util.Assert;

public class CustomMapEditor extends PropertyEditorSupport {

	private final Class<?> mapType;
	
	private final boolean nullAsEmptyMap;

	
	public CustomMapEditor(Class<?> mapType) {
		this(mapType, false);
	}
	
	public CustomMapEditor(Class<?> mapType, boolean nullAsEmptyMap) {
		Assert.notNull(mapType, "Map type is required");
		
		Assert.isAssignable(Map.class, mapType, 
			String.format("Map type [%s] does not implement [java.util.Map]", mapType.getName()));
		
		this.mapType = mapType;
		this.nullAsEmptyMap = nullAsEmptyMap;
	}

	@Override
	public void setValue(Object value) {
		if (value == null && nullAsEmptyMap) {
			super.setValue(createMap(mapType, 0));
		} 
		else if (value == null || (mapType.isInstance(value) && !alwaysCreateNewMap())) {
			super.setValue(value);
		}
		else if (value instanceof Map) {
			Map<?, ?> source = (Map<?, ?>) value;
			Map target = createMap(mapType, source.size());
			for (Map.Entry<?, ?> entry : source.entrySet()) {
				target.put(convertKey(entry.getKey()), convertValue(entry.getValue()));
			}
			super.setValue(target);
		}
		throw new IllegalArgumentException(String.format("Value cannot be converted to Map: ", value));
	}

	@Override
	public String getAsText() {
		return null;
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		setValue(text);
	}
	
	/**
	 * Create a Map of the given type, with the given
	 * initial capacity (if supported by the Map type).
	 * @param mapType
	 * @param initial
	 * @return
	 */
	protected Map createMap(Class<?> mapType, int initial) {
		if (!mapType.isInterface()) {
			try {
				return (Map) mapType.newInstance();
			} 
			catch (Exception ex) {
				throw new IllegalArgumentException(String.format(
					"Could not instantiate map class [%s]: %s", mapType.getName(), ex.getMessage()));
			}
		}
		if (SortedMap.class.equals(mapType)) {
			return new TreeMap();
		}
		return new LinkedHashMap(initial);
	}
	
	/**
	 * Return whether to always create a new Map,
	 * even if the type of the passed-in Map already matches.
	 * @return
	 */
	protected boolean alwaysCreateNewMap() {
		return false;
	}
	
	/**
	 * Hook to convert each encountered Map key.
	 * @param key
	 * @return
	 */
	protected Object convertKey(Object key) {
		return key;
	}
	
	/**
	 * Hook to convert each encountered Map value.
	 * @param value
	 * @return
	 */
	protected Object convertValue(Object value) {
		return value;
	}
}
