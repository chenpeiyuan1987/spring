package org.yuan.study.spring.beans;

import java.beans.PropertyEditor;
import java.io.File;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.SortedSet;

import org.springframework.core.io.Resource;

public class PropertyEditorRegistrySupport implements PropertyEditorRegistry {
	
	private Map<Class<?>,PropertyEditor> defaultEditors;
	
	private Map<Object,Object> customEditors;
	
	private Map<Class<?>,PropertyEditor> customEditorCache;
	
	//----------------------------------------------------------------------
	// Implementation methods
	//----------------------------------------------------------------------
	
	/**
	 * Copy the custom editors registered in this instance to the given target registry.
	 * @param target
	 * @param nestedProperty
	 */
	protected void copyCustomEditorsTo(PropertyEditorRegistry target, String nestedProperty) {
		String actualPropertyName = (nestedProperty != null ? PropertyAccessorUtils.getPropertyName(nestedProperty) : null);
		if (this.customEditors != null) {
			for (Entry<Object,Object> entry : this.customEditors.entrySet()) {
				if (entry.getKey() instanceof Class) {
					target.registerCustomEditor((Class<?>)entry.getKey(), (PropertyEditor)entry.getValue());
				}
				else if (entry.getKey() instanceof String & nestedProperty != null) {
					String editorPath = (String)entry.getKey();
					int pos = PropertyAccessorUtils.getFirstNestedPropertySeparatorIndex(editorPath);
					if (pos != -1) {
						String editorNestedProperty = editorPath.substring(0, pos);
						String editorNestedPath = editorPath.substring(pos + 1);
						if (editorNestedProperty.equals(nestedProperty) || editorNestedProperty.equals(actualPropertyName)) {
							CustomEditorHolder editorHolder = (CustomEditorHolder)entry.getValue();
							target.registerCustomEditor(editorHolder.getRegisteredType(), editorNestedPath, editorHolder.getPropertyEditor());
						}
					}
				}
			}
		}
	}
	
	/**
	 * Copy the default editors registered in this instance to the given target registry.
	 * @param target
	 */
	protected void copyDefaultEditorsTo(PropertyEditorRegistrySupport target) {
		target.defaultEditors = this.defaultEditors;
	}
	
	/**
	 * Retrieve the default editor for the given property type, if any.
	 * @param requiredType
	 * @return
	 */
	protected PropertyEditor getDefaultEditor(Class<?> requiredType) {
		if (this.defaultEditors == null) {
			return null;
		}
		return this.defaultEditors.get(requiredType);
	}
	
	/**
	 * Determine the property type of the given property path.
	 * @param propertyPath
	 * @return
	 */
	protected Class<?> getPropertyType(String propertyPath) {
		return null;
	}
	
	/**
	 * Guess the property type of the specified property from the registered custom editors.
	 * @param propertyName
	 * @return
	 */
	protected Class<?> guessPropertyTypeFromEditors(String propertyName) {
		if (this.customEditors != null) {
			CustomEditorHolder editorHolder = (CustomEditorHolder) this.customEditors.get(propertyName);
			if (editorHolder == null) {
				List<String> strippedPaths = new LinkedList<String>();
				addStrippedPropertyPaths(strippedPaths, "", propertyName);
				for (String strippedName : strippedPaths) {
					editorHolder = (CustomEditorHolder) this.customEditors.get(strippedName);
					if (editorHolder != null) {
						break;
					}
				}
			}
			if (editorHolder != null) {
				return editorHolder.getRegisteredType();
			}
		}
		return null;
	}
	
	/**
	 * Register default editors in this instance, for restricted environments
	 */
	protected void registerDefaultEditors() {
		this.defaultEditors = new HashMap<Class<?>, PropertyEditor>(32);
		
		this.defaultEditors.put(Class.class, null);
		this.defaultEditors.put(File.class, null);
		this.defaultEditors.put(InputStream.class, null);
		this.defaultEditors.put(Locale.class, null);
		this.defaultEditors.put(Properties.class, null);
		this.defaultEditors.put(Resource[].class, null);
		this.defaultEditors.put(String[].class, null);
		this.defaultEditors.put(URL.class, null);
		
		
		this.defaultEditors.put(Collection.class, null);
		this.defaultEditors.put(Set.class, null);
		this.defaultEditors.put(SortedSet.class, null);
		this.defaultEditors.put(List.class, null);
		
		
		this.defaultEditors.put(byte[].class, null);
		this.defaultEditors.put(char[].class, null);
		
		
		this.defaultEditors.put(char.class, null);
		this.defaultEditors.put(Character.class, null);
		this.defaultEditors.put(boolean.class, null);
		this.defaultEditors.put(Boolean.class, null);
		
		
		this.defaultEditors.put(byte.class, null);
		this.defaultEditors.put(Byte.class, null);
		this.defaultEditors.put(short.class, null);
		this.defaultEditors.put(Short.class, null);
		this.defaultEditors.put(int.class, null);
		this.defaultEditors.put(Integer.class, null);
		this.defaultEditors.put(long.class, null);
		this.defaultEditors.put(Long.class, null);
		this.defaultEditors.put(float.class, null);
		this.defaultEditors.put(Float.class, null);
		this.defaultEditors.put(double.class, null);
		this.defaultEditors.put(Double.class, null);
		this.defaultEditors.put(BigDecimal.class, null);
		this.defaultEditors.put(BigInteger.class, null);
	}
	
	/**
	 * Add property paths with all variations of stripped keys end/or indexes.
	 * @param strippedPaths
	 * @param nestedPath
	 * @param propertyPath
	 */
	private void addStrippedPropertyPaths(List<String> strippedPaths, String nestedPath, String propertyPath) {
		int startIndex = propertyPath.indexOf(PropertyAccessor.PROPERTY_KEY_PREFIX_CHAR);
		if (startIndex != -1) {
			int endIndex = propertyPath.indexOf(PropertyAccessor.PROPERTY_KEY_SUFFIX_CHAR);
			if (endIndex != -1) {
				String prefix = propertyPath.substring(0, startIndex);
				String key = propertyPath.substring(startIndex, endIndex + 1);
				String suffix = propertyPath.substring(endIndex + 1, propertyPath.length());
				
				strippedPaths.add(nestedPath + prefix + suffix);
				addStrippedPropertyPaths(strippedPaths, nestedPath + prefix, suffix);
				addStrippedPropertyPaths(strippedPaths, nestedPath + prefix + key, suffix);
			}
		}
	}
	
	/**
	 * Get custom editor for the given type. 
	 * If no direct match found, try custom editor for superclass.
	 * @param requiredType
	 * @return
	 */
	private PropertyEditor getCustomEditor(Class<?> requiredType) {
		if (requiredType == null) {
			return null;
		}
		
		PropertyEditor editor = (PropertyEditor) this.customEditors.get(requiredType);
		if (editor == null) {
			if (this.customEditorCache != null) {
				editor = (PropertyEditor) this.customEditorCache.get(requiredType);
			}
			if (editor == null) {
				for (Object key : this.customEditors.keySet()) {
					if (key instanceof Class && ((Class<?>)key).isAssignableFrom(requiredType)) {
						editor = (PropertyEditor) this.customEditors.get(key);
						if (this.customEditorCache == null) {
							this.customEditorCache = new HashMap<Class<?>, PropertyEditor>();
						}
						this.customEditorCache.put(requiredType, editor);
					}
				}
			}
		}
		
		return editor;
	}
	
	/**
	 * Get custom editor that has been registered for the given property.
	 * @param propertyName
	 * @param requiredType
	 * @return
	 */
	private PropertyEditor getCustomEditor(String propertyName, Class<?> requiredType) {
		CustomEditorHolder holder = (CustomEditorHolder) this.customEditors.get(propertyName);
		return (holder != null ? holder.getPropertyEditor(requiredType) : null);
	}
	
	
	//----------------------------------------------------------------------
	// Implementation of PropertyEditorRegistry interafce
	//----------------------------------------------------------------------

	@Override
	public PropertyEditor findCustomEditor(Class<?> requiredType, String propertyPath) {
		if (this.customEditors == null) {
			return null;
		}
		if (propertyPath != null) {
			PropertyEditor editor = getCustomEditor(propertyPath, requiredType);
			if (editor == null) {
				List<String> strippedPaths = new LinkedList<String>();
				addStrippedPropertyPaths(strippedPaths, "", propertyPath);
				for (String strippedPath : strippedPaths) {
					editor = getCustomEditor(strippedPath, requiredType);
				}
			}
			if (editor != null) {
				return editor;
			}
			else if (requiredType == null) {
				requiredType = getPropertyType(propertyPath);
			}
		}
		return getCustomEditor(requiredType);
	}

	@Override
	public void registerCustomEditor(Class<?> requiredType, PropertyEditor propertyEditor) {
		registerCustomEditor(requiredType, null, propertyEditor);
	}

	@Override
	public void registerCustomEditor(Class<?> requiredType, String propertyPath, PropertyEditor propertyEditor) {
		if (requiredType == null && propertyPath == null) {
			throw new IllegalArgumentException("Either requiredType or propertyPath is required");
		}
		if (this.customEditors == null) {
			this.customEditors = new HashMap<Object,Object>(16);
		}
		if (propertyPath != null) {
			this.customEditors.put(propertyPath, new CustomEditorHolder(propertyEditor, requiredType));
		}
		else {
			this.customEditors.put(requiredType, propertyEditor);
			this.customEditorCache = null;
		}
	}

	//----------------------------------------------------------------------
	// Implementation inner class
	//----------------------------------------------------------------------
	
	/**
	 * Holder for a registered custom editor with property name.
	 */
	private static class CustomEditorHolder {
		
		private final PropertyEditor propertyEditor;
		
		private final Class<?> registeredType;

		private CustomEditorHolder(PropertyEditor propertyEditor, Class<?> registeredType) {
			this.propertyEditor = propertyEditor;
			this.registeredType = registeredType;
		}

		private PropertyEditor getPropertyEditor() {
			return propertyEditor;
		}

		private Class<?> getRegisteredType() {
			return registeredType;
		}
		
		private PropertyEditor getPropertyEditor(Class<?> requiredType) {
			if (this.registeredType == null 
				|| (requiredType != null && (BeanUtils.isAssignable(this.registeredType, requiredType) || BeanUtils.isAssignable(requiredType, this.registeredType))
				|| (requiredType == null && (!Collection.class.isAssignableFrom(this.registeredType) && !this.registeredType.isAnnotation())))) {
				return this.propertyEditor;
			}
			else {
				return null;
			}
		}
	}
}
