package org.yuan.study.spring.beans;

import java.beans.PropertyEditor;
import java.lang.reflect.Modifier;
import java.util.Collection;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.yuan.study.spring.core.MethodParameter;
import org.yuan.study.spring.core.convert.TypeDescriptor;
import org.yuan.study.spring.util.ClassUtils;

class TypeConverterDelegate {

	private static final Log logger = LogFactory.getLog(TypeConverterDelegate.class);
	
	private final PropertyEditorRegistrySupport propertyEditorRegistry;
	
	private final Object targetObject;

	
	/**
	 * Create a new TypeConverterDelegate for the given editor registry.
	 * @param propertyEditorRegistry
	 * @param targetObject
	 */
	public TypeConverterDelegate(PropertyEditorRegistrySupport propertyEditorRegistry, Object targetObject) {
		this.propertyEditorRegistry = propertyEditorRegistry;
		this.targetObject = targetObject;
	}

	/**
	 * Create a new TypeConverterDelegate for the given editor registry.
	 * @param propertyEditorRegistry
	 */
	public TypeConverterDelegate(PropertyEditorRegistrySupport propertyEditorRegistry) {
		this(propertyEditorRegistry, null);
	}
	
	/**
	 * 
	 * @param newValue
	 * @param requiredType
	 * @param methodParam
	 * @return
	 * @throws IllegalArgumentException
	 */
	public <T> T convertIfNecessary(Object newValue, Class<T> requiredType, MethodParameter methodParam) throws IllegalArgumentException {
	}
	
	public <T> T convertIfNecessary(String propertyName, Object oldValue, Object newValue, Class<T> requiredType) throws IllegalArgumentException {
		
	}
	
	private Object attemptToConvertStringToEnum(Class<?> requiredType, String trimmedValue, Object currentConvertedValue) {
		
	}
	
	protected PropertyEditor findDefaultEditor(Class<?> requiredType, TypeDescriptor typeDescriptor) {
		
	}
	
	protected Object doConvertValue(Object oldValue, Object newValue, Class<?> requiredType, PropertyEditor editor) {
		
	}
	
	/**
	 * Convert the given text value using the given property editor.
	 * @param oldValue
	 * @param newTextValue
	 * @param editor
	 * @return
	 */
	protected Object doConvertTextValue(Object oldValue, String newTextValue, PropertyEditor editor) {
		try {
			editor.setValue(oldValue);
		} 
		catch (Exception e) {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("PropertyEditor [%s] does not support setValue call", editor.getClass().getName()), ex);
			}
		}
		editor.setAsText(newTextValue);
		return editor.getValue();
	}
	
	protected Object convertToTypedArray(Object input, String propertyName, Class<?> componentType) {
		
	}
	
	protected Collection convertToTypedCollection() {
		
	}
	
	protected Map convertToTypedMap() {
		
	}
	
	private String buildIndexedPropertyName(String propertyName, int index) {
		if (propertyName == null) {
			return null;
		}
		
		return propertyName + PropertyAccessor.PROPERTY_KEY_PREFIX + index + PropertyAccessor.PROPERTY_KEY_SUFFIX;
	}
	
	private String buildKeyedPropertyName(String propertyName, Object key) {
		if (propertyName == null) {
			return null;
		}
		
		return propertyName + PropertyAccessor.PROPERTY_KEY_PREFIX + key + PropertyAccessor.PROPERTY_KEY_SUFFIX;
	}
	
	private boolean canCreateCopy(Class<?> requiredType) {
		if (requiredType.isInterface()) {
			return false;
		}
		if (Modifier.isAbstract(requiredType.getModifiers())) {
			return false;
		}
		if (!Modifier.isPublic(requiredType.getModifiers())) {
			return false;
		}
		if (!ClassUtils.hasConstructor(requiredType)) {
			return false;
		}
		
		return true;
	}
}
