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

import javax.swing.plaf.basic.DefaultMenuLayout;

import org.yuan.study.spring.beans.propertyeditors.ByteArrayPropertyEditor;
import org.yuan.study.spring.beans.propertyeditors.CharArrayPropertyEditor;
import org.yuan.study.spring.beans.propertyeditors.CharacterEditor;
import org.yuan.study.spring.beans.propertyeditors.ClassEditor;
import org.yuan.study.spring.beans.propertyeditors.CustomBooleanEditor;
import org.yuan.study.spring.beans.propertyeditors.CustomCollectionEditor;
import org.yuan.study.spring.beans.propertyeditors.CustomNumberEditor;
import org.yuan.study.spring.beans.propertyeditors.FileEditor;
import org.yuan.study.spring.beans.propertyeditors.InputStreamEditor;
import org.yuan.study.spring.beans.propertyeditors.LocaleEditor;
import org.yuan.study.spring.beans.propertyeditors.PropertiesEditor;
import org.yuan.study.spring.beans.propertyeditors.StringArrayPropertyEditor;
import org.yuan.study.spring.beans.propertyeditors.URLEditor;
import org.yuan.study.spring.core.convert.ConversionService;
import org.yuan.study.spring.core.io.Resource;
import org.yuan.study.spring.core.io.ResourceEditor;
import org.yuan.study.spring.util.ClassUtils;

public class PropertyEditorRegistrySupport implements PropertyEditorRegistry {
	
	private ConversionService conversionService;
	
	private boolean defaultEditorsActive = false;
	
	private boolean configValueEditorsActive = false;
	
	private Map<Class<?>, PropertyEditor> defaultEditors;

	private Map<Class<?>, PropertyEditor> overriddenDefaultEditors;
	
	private Map<Class<?>, Object> customEditors;
	
	private Map<String, CustomEditorHolder> customEditorForPath;
	
	private Set<PropertyEditor> sharedEditors;
	
	private Map<Class<?>, PropertyEditor> customEditorCache;
	
	
	//----------------------------------------------------------------------
	// Implementation methods
	//----------------------------------------------------------------------
	
	/**
	 * Return the associated ConversionService, if any.
	 */
	public ConversionService getConversionService() {
		return conversionService;
	}

	/**
	 * Specify a Spring 3.0 ConversionService to use for converting
	 * property values, as an alternative to JavaBeans PropertyEditors.
	 */
	public void setConversionService(ConversionService conversionService) {
		this.conversionService = conversionService;
	}

	/**
	 * Activate the default editors for this registry instance,
	 * allowing for lazily registering default editors when needed.
	 */
	protected void registerDefaultEditors() {
		defaultEditorsActive = true;
	}

	/**
	 * Activate config value editors which are only intended for configuration purpose,
	 * such as StringArrayPropertyEditor.
	 */
	public void useConfigValueEditors() {
		configValueEditorsActive = true;
	}
	
	/**
	 * Override the default editor for the specified type with the given property editor.
	 * @param requiredType
	 * @param propertyEditor
	 */
	public void overrideDefaultEditor(Class<?> requiredType, PropertyEditor propertyEditor) {
		if (overriddenDefaultEditors == null) {
			overriddenDefaultEditors = new HashMap<Class<?>, PropertyEditor>();
		}
		overriddenDefaultEditors.put(requiredType, propertyEditor);
	}
	
	/**
	 * Retrieve the default editor for the given property type, if any.
	 * @param requiredType
	 * @return
	 */
	public PropertyEditor getDefaultEditor(Class<?> requiredType) {
		if (!defaultEditorsActive) {
			return null;
		}
		if (overriddenDefaultEditors != null) {
			PropertyEditor editor = overriddenDefaultEditors.get(requiredType);
			if (editor != null) {
				return editor;
			}
		}
		if (defaultEditors == null) {
			createDefaultEditors();
		}
		return defaultEditors.get(requiredType);
	}
	
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
	 * Actually register the default editors for this registry instance.
	 */
	private void createDefaultEditors() {
		this.defaultEditors = new HashMap<Class<?>, PropertyEditor>(32);
		
		this.defaultEditors.put(Class.class, new ClassEditor());
		this.defaultEditors.put(File.class, new FileEditor());
		this.defaultEditors.put(InputStream.class, new InputStreamEditor());
		this.defaultEditors.put(Locale.class, new LocaleEditor());
		this.defaultEditors.put(Properties.class, new PropertiesEditor());
		this.defaultEditors.put(Resource[].class, new ResourceEditor());
		this.defaultEditors.put(String[].class, new StringArrayPropertyEditor());
		this.defaultEditors.put(URL.class, new URLEditor());
		
		
		this.defaultEditors.put(Collection.class, new CustomCollectionEditor(Collection.class));
		this.defaultEditors.put(Set.class, new CustomCollectionEditor(Set.class));
		this.defaultEditors.put(SortedSet.class, new CustomCollectionEditor(SortedSet.class));
		this.defaultEditors.put(List.class, new CustomCollectionEditor(List.class));
		
		
		this.defaultEditors.put(byte[].class, new ByteArrayPropertyEditor());
		this.defaultEditors.put(char[].class, new CharArrayPropertyEditor());
		
		
		PropertyEditor characterEditor = new CharacterEditor(false);
		PropertyEditor booleanEditor = new CustomBooleanEditor(false);
		this.defaultEditors.put(char.class, characterEditor);
		this.defaultEditors.put(Character.class, characterEditor);
		this.defaultEditors.put(boolean.class, booleanEditor);
		this.defaultEditors.put(Boolean.class, booleanEditor);
		
		
		PropertyEditor byteEditor = new CustomNumberEditor(Byte.class, false);
		PropertyEditor shortEditor = new CustomNumberEditor(Short.class, false);
		PropertyEditor integerEditor = new CustomNumberEditor(Integer.class, false);
		PropertyEditor longEditor = new CustomNumberEditor(Long.class, false);
		PropertyEditor floatEditor = new CustomNumberEditor(Float.class, false);
		PropertyEditor doubleEditor = new CustomNumberEditor(Double.class, false);
		this.defaultEditors.put(byte.class, byteEditor);
		this.defaultEditors.put(Byte.class, byteEditor);
		this.defaultEditors.put(short.class, shortEditor);
		this.defaultEditors.put(Short.class, shortEditor);
		this.defaultEditors.put(int.class, integerEditor);
		this.defaultEditors.put(Integer.class, integerEditor);
		this.defaultEditors.put(long.class, longEditor);
		this.defaultEditors.put(Long.class, longEditor);
		this.defaultEditors.put(float.class, floatEditor);
		this.defaultEditors.put(Float.class, floatEditor);
		this.defaultEditors.put(double.class, doubleEditor);
		this.defaultEditors.put(Double.class, doubleEditor);
		this.defaultEditors.put(BigDecimal.class, new CustomNumberEditor(BigDecimal.class, false));
		this.defaultEditors.put(BigInteger.class, new CustomNumberEditor(BigInteger.class, false));
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
	 * Keeps the PropertyEditor itself plus the type it was registered for.
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
			if (registeredType == null 
				|| (requiredType != null && (ClassUtils.isAssignable(registeredType, requiredType) || ClassUtils.isAssignable(requiredType, registeredType)))
				|| (requiredType == null && (!Collection.class.isAssignableFrom(registeredType) && !registeredType.isArray()))) {
				return propertyEditor;
			}
			else {
				return null;
			}
		}
	}
}
