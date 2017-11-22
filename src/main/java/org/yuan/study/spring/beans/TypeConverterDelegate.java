package org.yuan.study.spring.beans;

import java.beans.PropertyDescriptor;
import java.beans.PropertyEditor;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Collection;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.yuan.study.spring.core.CollectionFactory;
import org.yuan.study.spring.core.MethodParameter;
import org.yuan.study.spring.core.convert.TypeDescriptor;
import org.yuan.study.spring.core.convert.support.PropertyTypeDescriptor;
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
	 * Convert the value to the specified required type.
	 * @param newValue
	 * @param requiredType
	 * @param methodParam
	 * @return
	 * @throws IllegalArgumentException
	 */
	public <T> T convertIfNecessary(Object newValue, Class<T> requiredType, MethodParameter methodParam) throws IllegalArgumentException {
		return convertIfNecessary(null, null, newValue, requiredType, 
			(methodParam != null ? new TypeDescriptor(methodParam) : TypeDescriptor.valueOf(requiredType)));
	}
	
	/**
	 * Convert the value to the required type for the specified property.
	 * @param propertyName
	 * @param oldValue
	 * @param newValue
	 * @param requiredType
	 * @return
	 * @throws IllegalArgumentException
	 */
	public <T> T convertIfNecessary(String propertyName, Object oldValue, Object newValue, Class<T> requiredType) throws IllegalArgumentException {
		return convertIfNecessary(propertyName, oldValue, newValue, requiredType, TypeDescriptor.valueOf(requiredType));
	}
	
	/**
	 * Convert the value to the required type for the specified property.
	 * @param propertyName
	 * @param oldValue
	 * @param newValue
	 * @param requiredType
	 * @param typeDescriptor
	 * @return
	 * @throws IllegalArgumentException
	 */
	public <T> T convertIfNecessary(String propertyName, Object oldValue, Object newValue, Class<T> requiredType, TypeDescriptor typeDescriptor) throws IllegalArgumentException {
		
	}
	
	private Object attemptToConvertStringToEnum(Class<?> requiredType, String trimmedValue, Object currentConvertedValue) {
		Object convertedValue = currentConvertedValue;
		
		if (Enum.class.equals(requiredType)) {
			int index = trimmedValue.lastIndexOf(".");
			if (index > -1) {
				String enumType = trimmedValue.substring(0, index);
				String fieldName = trimmedValue.substring(index + 1);
				ClassLoader loader = targetObject.getClass().getClassLoader();
				try {
					Class<?> enumValueType = loader.loadClass(enumType);
					Field enumField = enumValueType.getField(fieldName);
					convertedValue = enumField.get(null);
				} 
				catch (ClassNotFoundException ex) {
					if (logger.isTraceEnabled()) {
						logger.trace(String.format(
							"Enum class [%s] cannot be loaded from [%s]", enumType, loader), ex);
					}
				}
				catch (Throwable ex) {
					if (logger.isTraceEnabled()) {
						logger.trace(String.format(
							"field [%s] isn't an enum value for type [%s]", fieldName, enumType), ex);
					}
				}
			}
		}
		
		if (convertedValue == currentConvertedValue) {
			try {
				Field enumField = requiredType.getField(trimmedValue);
				convertedValue = enumField.get(null);
			} catch (Throwable ex) {
				if (logger.isTraceEnabled()) {
					logger.trace(String.format("Field [%s] isn't an enum value", convertedValue), ex);
				}
			}
		}
		
		return convertedValue;
	}
	
	protected PropertyEditor findDefaultEditor(Class<?> requiredType, TypeDescriptor typeDescriptor) {
		PropertyEditor editor = null;
		if (typeDescriptor instanceof PropertyTypeDescriptor) {
			PropertyDescriptor pd = ((PropertyTypeDescriptor) typeDescriptor).
		}
		if (editor == null && requiredType != null) {
			editor = propertyEditorRegistry.getDefaultEditor(requiredType);
			if (editor == null && !String.class.equals(requiredType)) {
				editor = BeanUtils.findEditorByConvention(targetType);
			}
		}
		return editor;
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
	
	protected Collection<?> convertToTypedCollection(Collection<?> original, String propertyName, Class<?> requiredType, TypeDescriptor typeDescriptor) {
		if (!Collection.class.isAssignableFrom(requiredType)) {
			return original;
		}
		
		boolean approximable = CollectionFactory.isApproximableCollectionType(requiredType);
		if (!approximable && !canCreateCopy(requiredType)) {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format(
					"Custom Collection type [%s] does not allow for creating a copy - injecting original Collection as-is", 
						original.getClass().getName()));
			}
		}
		
		boolean originalAllowed = requiredType.isInstance(original);
		MethodParameter methodParam = typeDescriptor.getMethodParameter();
	}
	
	protected Map<?, ?> convertToTypedMap(Map<?, ?> original, String propertyName, Class<?> requiredType, TypeDescriptor typeDescriptor) {
		if (!Map.class.isAssignableFrom(requiredType)) {
			return original;
		}
		
		boolean approximable = CollectionFactory.isApproximableMapType(requiredType);
		if (!approximable && !canCreateCopy(requiredType)) {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format(
					"Custom Map type [%s] does not allow for creating a copy - injecting original Map as-is", 
						original.getClass().getName()));
			}
			return original;
		}
		
		boolean originalAllowed = requiredType.isInstance(original);
		Class<?> keyType = null;
		Class<?> valType = null;
		MethodParameter methodParam = typeDescriptor.get
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
