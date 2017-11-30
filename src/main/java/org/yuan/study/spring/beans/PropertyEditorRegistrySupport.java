package org.yuan.study.spring.beans;

import java.beans.PropertyEditor;
import java.io.File;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.Collection;
import java.util.Currency;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TimeZone;
import java.util.UUID;
import java.util.regex.Pattern;

import org.xml.sax.InputSource;
import org.yuan.study.spring.beans.propertyeditors.ByteArrayPropertyEditor;
import org.yuan.study.spring.beans.propertyeditors.CharArrayPropertyEditor;
import org.yuan.study.spring.beans.propertyeditors.CharacterEditor;
import org.yuan.study.spring.beans.propertyeditors.CharsetEditor;
import org.yuan.study.spring.beans.propertyeditors.ClassArrayEditor;
import org.yuan.study.spring.beans.propertyeditors.ClassEditor;
import org.yuan.study.spring.beans.propertyeditors.CurrencyEditor;
import org.yuan.study.spring.beans.propertyeditors.CustomBooleanEditor;
import org.yuan.study.spring.beans.propertyeditors.CustomCollectionEditor;
import org.yuan.study.spring.beans.propertyeditors.CustomMapEditor;
import org.yuan.study.spring.beans.propertyeditors.CustomNumberEditor;
import org.yuan.study.spring.beans.propertyeditors.FileEditor;
import org.yuan.study.spring.beans.propertyeditors.InputSourceEditor;
import org.yuan.study.spring.beans.propertyeditors.InputStreamEditor;
import org.yuan.study.spring.beans.propertyeditors.LocaleEditor;
import org.yuan.study.spring.beans.propertyeditors.PatternEditor;
import org.yuan.study.spring.beans.propertyeditors.PropertiesEditor;
import org.yuan.study.spring.beans.propertyeditors.StringArrayPropertyEditor;
import org.yuan.study.spring.beans.propertyeditors.TimeZoneEditor;
import org.yuan.study.spring.beans.propertyeditors.URIEditor;
import org.yuan.study.spring.beans.propertyeditors.URLEditor;
import org.yuan.study.spring.beans.propertyeditors.UUIDEditor;
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
	
	private Map<Class<?>, PropertyEditor> customEditors;
	
	private Map<String, CustomEditorHolder> customEditorsForPath;
	
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
	 * Actually register the default editors for this registry instance.
	 */
	private void createDefaultEditors() {
		this.defaultEditors = new HashMap<Class<?>, PropertyEditor>(64);
		
		this.defaultEditors.put(Charset.class, new CharsetEditor());
		this.defaultEditors.put(Class.class, new ClassEditor());
		this.defaultEditors.put(Class[].class, new ClassArrayEditor());
		this.defaultEditors.put(Currency.class, new CurrencyEditor());
		this.defaultEditors.put(File.class, new FileEditor());
		this.defaultEditors.put(InputStream.class, new InputStreamEditor());
		this.defaultEditors.put(InputSource.class, new InputSourceEditor());
		this.defaultEditors.put(Locale.class, new LocaleEditor());
		this.defaultEditors.put(Pattern.class, new PatternEditor());
		this.defaultEditors.put(Properties.class, new PropertiesEditor());
		this.defaultEditors.put(Resource[].class, new ResourceEditor());
		this.defaultEditors.put(TimeZone.class, new TimeZoneEditor());
		this.defaultEditors.put(URI.class, new URIEditor());
		this.defaultEditors.put(URL.class, new URLEditor());
		this.defaultEditors.put(UUID.class, new UUIDEditor());
		
		
		this.defaultEditors.put(Collection.class, new CustomCollectionEditor(Collection.class));
		this.defaultEditors.put(Set.class, new CustomCollectionEditor(Set.class));
		this.defaultEditors.put(SortedSet.class, new CustomCollectionEditor(SortedSet.class));
		this.defaultEditors.put(List.class, new CustomCollectionEditor(List.class));
		this.defaultEditors.put(SortedMap.class, new CustomMapEditor(SortedMap.class));
		
		
		this.defaultEditors.put(byte[].class, new ByteArrayPropertyEditor());
		this.defaultEditors.put(char[].class, new CharArrayPropertyEditor());
		
		
		this.defaultEditors.put(char.class, new CharacterEditor(false));
		this.defaultEditors.put(Character.class, new CharacterEditor(true));
		this.defaultEditors.put(boolean.class, new CustomBooleanEditor(false));
		this.defaultEditors.put(Boolean.class, new CustomBooleanEditor(true));
		
		
		this.defaultEditors.put(byte.class, new CustomNumberEditor(Byte.class, false));
		this.defaultEditors.put(Byte.class, new CustomNumberEditor(Byte.class, true));
		this.defaultEditors.put(short.class, new CustomNumberEditor(Short.class, false));
		this.defaultEditors.put(Short.class, new CustomNumberEditor(Short.class, true));
		this.defaultEditors.put(int.class, new CustomNumberEditor(Integer.class, false));
		this.defaultEditors.put(Integer.class, new CustomNumberEditor(Integer.class, true));
		this.defaultEditors.put(long.class, new CustomNumberEditor(Long.class, false));
		this.defaultEditors.put(Long.class, new CustomNumberEditor(Long.class, true));
		this.defaultEditors.put(float.class, new CustomNumberEditor(Float.class, false));
		this.defaultEditors.put(Float.class, new CustomNumberEditor(Float.class, true));
		this.defaultEditors.put(double.class, new CustomNumberEditor(Double.class, false));
		this.defaultEditors.put(Double.class, new CustomNumberEditor(Double.class, true));
		this.defaultEditors.put(BigDecimal.class, new CustomNumberEditor(BigDecimal.class, true));
		this.defaultEditors.put(BigInteger.class, new CustomNumberEditor(BigInteger.class, true));
		
		
		if (this.configValueEditorsActive) {
			StringArrayPropertyEditor sae = new StringArrayPropertyEditor();
			this.defaultEditors.put(String[].class, sae);
			this.defaultEditors.put(short[].class, sae);
			this.defaultEditors.put(int[].class, sae);
			this.defaultEditors.put(long[].class, sae);
		}
	}
	
	/**
	 * Copy the default editors registered in this instance to the given target registry.
	 * @param target
	 */
	protected void copyDefaultEditorsTo(PropertyEditorRegistrySupport target) {
		target.defaultEditorsActive = target.defaultEditorsActive;
		target.configValueEditorsActive = this.configValueEditorsActive;
		target.defaultEditors = this.defaultEditors;
		target.overriddenDefaultEditors = this.overriddenDefaultEditors;
	}
	
	/**
	 * Copy the custom editors registered in this instance to the given target registry.
	 * @param target
	 * @param nestedProperty
	 */
	protected void copyCustomEditorsTo(PropertyEditorRegistry target, String nestedProperty) {
		String actualPropertyName = (nestedProperty != null ? PropertyAccessorUtils.getPropertyName(nestedProperty) : null);
		if (customEditors != null) {
			for (Entry<Class<?>, PropertyEditor> entry : customEditors.entrySet()) {
				target.registerCustomEditor(entry.getKey(), entry.getValue());
			}
		}
		if (customEditorsForPath != null) {
			for (Entry<String, CustomEditorHolder> entry : this.customEditorsForPath.entrySet()) {
				String editorPath = entry.getKey();
				CustomEditorHolder editorHolder = entry.getValue();
				if (nestedProperty != null) {
					int pos = PropertyAccessorUtils.getFirstNestedPropertySeparatorIndex(editorPath);
					if (pos != -1) {
						String editorNestedProperty = editorPath.substring(0, pos);
						String editorNestedPath = editorPath.substring(pos + 1);
						if (editorNestedProperty.equals(nestedProperty) || editorNestedProperty.equals(actualPropertyName)) {
							target.registerCustomEditor(editorHolder.getRegisteredType(), editorNestedPath, editorHolder.getPropertyEditor());
						}
					}
				}
				else {
					target.registerCustomEditor(editorHolder.getRegisteredType(), editorPath, editorHolder.getPropertyEditor());
				}
			}
		}
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
		if (this.customEditorsForPath != null) {
			CustomEditorHolder editorHolder = this.customEditorsForPath.get(propertyName);
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
		if (requiredType == null || this.customEditors == null) {
			return null;
		}
		
		PropertyEditor editor = this.customEditors.get(requiredType);
		if (editor == null) {
			if (this.customEditorCache != null) {
				editor = this.customEditorCache.get(requiredType);
			}
			if (editor == null) {
				for (Class<?> key : this.customEditors.keySet()) {
					if (key.isAssignableFrom(requiredType)) {
						editor = this.customEditors.get(key);
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
		CustomEditorHolder holder = customEditorsForPath.get(propertyName);
		return (holder != null ? holder.getPropertyEditor(requiredType) : null);
	}
	
	//----------------------------------------------------------------------
	// Implementation of PropertyEditorRegistry interafce
	//----------------------------------------------------------------------

	@Override
	public PropertyEditor findCustomEditor(Class<?> requiredType, String propertyPath) {
		Class<?> requiredTypeToUse = requiredType;
		if (propertyPath != null) {
			if (customEditorsForPath != null) {
				PropertyEditor editor = getCustomEditor(propertyPath, requiredType);
				if (editor == null) {
					List<String> strippedPaths = new LinkedList<String>();
					addStrippedPropertyPaths(strippedPaths, "", propertyPath);
					for (String strippedPath : strippedPaths) {
						editor = getCustomEditor(strippedPath, requiredType);
						if (editor != null) {
							break;
						}
					}
				}
				if (editor != null) {
					return editor;
				}
			}
			if (requiredType == null) {
				requiredTypeToUse = getPropertyType(propertyPath);
			}
		}
		return getCustomEditor(requiredTypeToUse);
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
		if (propertyPath != null) {
			if (customEditorsForPath == null) {
				customEditorsForPath = new LinkedHashMap<String, CustomEditorHolder>(16);
			}
			customEditorsForPath.put(propertyPath, new CustomEditorHolder(propertyEditor, requiredType));
		}
		else {
			if (this.customEditors == null) {
				this.customEditors = new LinkedHashMap<Class<?>, PropertyEditor>(16);
			}
			this.customEditors.put(requiredType, propertyEditor);
			this.customEditorCache = null;
		}
	}
	
	/**
	 * Register the given custom property editor for all properties
	 * of the given type, indicating that the given instance is a 
	 * shared editor that might be used concurrently.
	 * @param requiredType
	 * @param propertyEditor
	 */
	public void registerSharedEditor(Class<?> requiredType, PropertyEditor propertyEditor) {
		registerCustomEditor(requiredType, null, propertyEditor);
		if (sharedEditors == null) {
			sharedEditors = new HashSet<PropertyEditor>();
		}
		sharedEditors.add(propertyEditor);
	}

	/**
	 * Check whether the given editor instance is a shared editor, that is,
	 * whether the given editor instance might be used concurrently.
	 * @param propertyEditor
	 * @return
	 */
	public boolean isSharedEditor(PropertyEditor propertyEditor) {
		return (sharedEditors != null && sharedEditors.contains(propertyEditor));
	}
	
	/**
	 * Determine whether this registry contains a custom editor
	 * for the specified array/collection element.
	 * @param elementType
	 * @param propertyPath
	 * @return
	 */
	public boolean hasCustomEditorForElement(Class<?> elementType, String propertyPath) {
		if (propertyPath != null && customEditorsForPath != null) {
			for (Entry<String, CustomEditorHolder> entry : customEditorsForPath.entrySet()) {
				if (PropertyAccessorUtils.matchesProperty(entry.getKey(), propertyPath)) {
					if (entry.getValue().getPropertyEditor(elementType) != null) {
						return true;
					}
				}
			}
		}
		
		return (elementType != null && customEditors != null && customEditors.containsKey(elementType));
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
