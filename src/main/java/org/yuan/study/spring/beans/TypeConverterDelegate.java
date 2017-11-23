package org.yuan.study.spring.beans;

import java.beans.PropertyDescriptor;
import java.beans.PropertyEditor;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.yuan.study.spring.core.CollectionFactory;
import org.yuan.study.spring.core.GenericCollectionTypeResolver;
import org.yuan.study.spring.core.MethodParameter;
import org.yuan.study.spring.core.convert.ConversionService;
import org.yuan.study.spring.core.convert.TypeDescriptor;
import org.yuan.study.spring.core.convert.support.PropertyTypeDescriptor;
import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.StringUtils;

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
		Object convertedValue = newValue;
		
		PropertyEditor editor = propertyEditorRegistry.findCustomEditor(requiredType, propertyName);
		
		ConversionService conversionService = propertyEditorRegistry.getConversionService();
		if (editor == null && conversionService != null && convertedValue != null) {
			TypeDescriptor sourceTypeDesc = TypeDescriptor.forObject(convertedValue);
			TypeDescriptor targetTypeDesc = typeDescriptor;
			if (requiredType != null && !requiredType.isAssignableFrom(typeDescriptor.getType())) {
				targetTypeDesc = typeDescriptor.forElementType(requiredType);
			}
			if (conversionService.canConvert(sourceTypeDesc, targetTypeDesc)) {
				return (T) conversionService.convert(convertedValue, sourceTypeDesc, targetTypeDesc);
			}
		}
		
		if (editor != null || (requiredType != null && !ClassUtils.isAssignableValue(requiredType, convertedValue))) {
			if (requiredType != null && Collection.class.isAssignableFrom(requiredType) && convertedValue instanceof String && typeDescriptor.getMethodParameter() != null) {
				Class<?> elemType = GenericCollectionTypeResolver.getCollectionParameterType(typeDescriptor.getMethodParameter());
				if (elemType != null && Enum.class.isAssignableFrom(elemType)) {
					convertedValue = StringUtils.commaDelimitedListToStringArray((String)convertedValue);
				}
			}
			if (editor == null) {
				editor = findDefaultEditor(requiredType, typeDescriptor);
			}
			convertedValue = doConvertValue(oldValue, convertedValue, requiredType, editor);
		}
		
		if (requiredType != null) {
			if (convertedValue != null) {
				if (requiredType.isArray()) {
					if (convertedValue instanceof String && Enum.class.isAssignableFrom(requiredType.getComponentType())) {
						convertedValue = StringUtils.commaDelimitedListToStringArray((String)convertedValue);
					}
					return (T) convertToTypedArray(convertedValue, propertyName, requiredType.getComponentType());
				}
				else if (convertedValue instanceof Collection) {
					convertedValue = convertToTypedCollection(
						(Collection<?>) convertedValue, propertyName, requiredType, typeDescriptor);
				}
				else if (convertedValue instanceof Map) {
					convertedValue = convertToTypedMap(
						(Map<?, ?>) convertedValue, propertyName, requiredType, typeDescriptor);
				}
				
				if (convertedValue.getClass().isArray() && Array.getLength(convertedValue) == 1) {
					convertedValue = Array.get(convertedValue, 0);
				}
				if (String.class.equals(requiredType) && ClassUtils.isPrimitiveOrWrapper(convertedValue.getClass())) {
					return (T) convertedValue.toString();
				}
				else if (convertedValue instanceof String && !requiredType.isInstance(convertedValue)) {
					if (!requiredType.isInterface() && !requiredType.isEnum()) {
						try {
							Constructor<?> strCtor = requiredType.getConstructor(String.class);
							return (T) BeanUtils.instantiateClass(strCtor, convertedValue);
						} 
						catch (NoSuchMethodException ex) {
							if (logger.isTraceEnabled()) {
								logger.trace(String.format(
									"No String constructor found on type [%s]", requiredType.getName()), ex);
							}
						}
						catch (Exception ex) {
							if (logger.isDebugEnabled()) {
								logger.debug(String.format(
									"Construction via String failed for type [%s]", requiredType.getName()), ex);
							}
						}
					}
					String trimmedValue = ((String) convertedValue).trim();
					if (requiredType.isEnum() && "".equals(trimmedValue)) {
						return null;
					}
					convertedValue = attemptToConvertStringToEnum(requiredType, trimmedValue, convertedValue);
				}
			}
			
			if (!ClassUtils.isAssignableValue(requiredType, convertedValue)) {
				StringBuilder msg = new StringBuilder();
				msg.append("Cannot convert value of type [");
				msg.append(ClassUtils.getDescriptiveType(newValue));
				msg.append("] to required type [");
				msg.append(ClassUtils.getQualifiedName(requiredType));
				msg.append("]");
				if (propertyName != null) {
					msg.append(" for property '").append(propertyName).append("'");
				}
				if (editor != null) {
					msg.append(": PropertyEditor [");
					msg.append(editor.getClass().getName());
					msg.append("] returned inappropriate value of type [");
					msg.append(ClassUtils.getDescriptiveType(convertedValue));
					msg.append("]");
					throw new IllegalArgumentException(msg.toString());
				}
				else {
					msg.append(": no matching editors or conversion strategy found");
					throw new IllegalStateException(msg.toString());
				}
			}
		}
		
		return (T) convertedValue;
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
			PropertyDescriptor pd = ((PropertyTypeDescriptor) typeDescriptor).getPropertyDescriptor();
			editor = pd.createPropertyEditor(targetObject);
		}
		if (editor == null && requiredType != null) {
			editor = propertyEditorRegistry.getDefaultEditor(requiredType);
			if (editor == null && !String.class.equals(requiredType)) {
				editor = BeanUtils.findEditorByConvention(requiredType);
			}
		}
		return editor;
	}
	
	protected Object doConvertValue(Object oldValue, Object newValue, Class<?> requiredType, PropertyEditor editor) {
		Object convertedValue = newValue;
		boolean sharedEditor = false;
		
		if (editor != null) {
			sharedEditor = propertyEditorRegistry.isSharedEditor(editor);
		}
		
		if (editor != null && !(convertedValue instanceof String)) {
			try {
				Object newConvertedValue;
				if (sharedEditor) {
					synchronized (editor) {
						editor.setValue(convertedValue);
						newConvertedValue = editor.getValue();
					}
				}
				else {
					editor.setValue(convertedValue);
					newConvertedValue = editor.getValue();
				}
				if (newConvertedValue != convertedValue) {
					convertedValue = newConvertedValue;
					editor = null;
				}
			} catch (Exception ex) {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format(
						"PropertyEditor [%s] does not support setValue call", 
							editor.getClass().getName()), ex);
				}
			}
		}
		
		Object returnValue = convertedValue;
		
		if (requiredType != null && !requiredType.isArray() && convertedValue instanceof String[]) {
			if (logger.isTraceEnabled()) {
				logger.trace(String.format("Converting String array to comma-delimited String [%s]", convertedValue));
			}
			convertedValue = StringUtils.arrayToCommaDelimitedString((String[]) convertedValue);
		}
		
		if (convertedValue instanceof String) {
			if (editor != null) {
				if (logger.isTraceEnabled()) {
					logger.trace(String.format(
						"Converting String to [%s] using property editor [%s]", requiredType, editor));
				}
				String newTextValue = (String) convertedValue;
				if (sharedEditor) {
					synchronized (editor) {
						return doConvertTextValue(oldValue, newTextValue, editor);
					}
				}
				else {
					return doConvertTextValue(oldValue, newTextValue, editor);
				}
			}
			else if (String.class.equals(requiredType)) {
				returnValue = convertedValue;
			}
		}
		
		return returnValue;
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
		catch (Exception ex) {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("PropertyEditor [%s] does not support setValue call", editor.getClass().getName()), ex);
			}
		}
		editor.setAsText(newTextValue);
		return editor.getValue();
	}
	
	protected Object convertToTypedArray(Object input, String propertyName, Class<?> componentType) {
		if (input instanceof Collection) {
			Collection<?> coll = (Collection<?>) input;
			Object result = Array.newInstance(componentType, coll.size());
			int i = 0;
			for (Iterator<?> it = coll.iterator(); it.hasNext(); i++) {
				Object value = convertIfNecessary(
					buildIndexedPropertyName(propertyName, i), null, it.next(), componentType);
				Array.set(result, i, value);
			}
			return result;
		}
		
		if (input.getClass().isArray()) {
			if (componentType.equals(input.getClass().getComponentType()) && 
				!propertyEditorRegistry.hasCustomEditorForElement(componentType, propertyName)) {
				return input;
			}
			int arrayLength = Array.getLength(input);
			Object result = Array.newInstance(componentType, arrayLength);
			for (int i = 0; i < arrayLength; i++) {
				Object value = convertIfNecessary(
					buildIndexedPropertyName(propertyName, i), null, Array.get(input, i), componentType);
				Array.set(result, i, value);
			}
			return result;
		}
		
		Object result = Array.newInstance(componentType, 1);
		Object value = convertIfNecessary(buildIndexedPropertyName(propertyName, 0), null, input, componentType);
		Array.set(result, 0, value);
		return result;
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
		Class<?> elementType = null;
		if (methodParam != null) {
			elementType = GenericCollectionTypeResolver.getCollectionParameterType(methodParam);
		}
		if (elementType == null && originalAllowed 
			&& !propertyEditorRegistry.hasCustomEditorForElement(null, propertyName)) {
			return original;
		}
		
		Iterator it;
		try {
			it = original.iterator();
			if (it == null) {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format(
						"Collection of type [%s] returned null Iterator - injecting original Collection as-is", 
							original.getClass().getName()));
				}
				return original;
			}
		} 
		catch (Throwable ex) {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format(
					"Cannot access Collection of type [%s] - injecting original Collection as-is: %s", 
						original.getClass().getName(), ex));
			}
			return original;
		}
		
		Collection<Object> convertedCopy;
		try {
			if (approximable) {
				convertedCopy = CollectionFactory.createApproximateCollection(original, original.size());
			}
			else {
				convertedCopy = (Collection<Object>) requiredType.newInstance();
			}
		} 
		catch (Throwable ex) {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format(
					"Cannot create copy of Collection type [%s] - injecting original Collection as-is: %s", 
						original.getClass().getName(), ex));
			}
			return original;
		}
		
		int i = 0;
		for (; it.hasNext(); i++) {
			Object element = it.next();
			String indexedPropertyName = buildIndexedPropertyName(propertyName, i);
			if (methodParam != null) {
				methodParam.decreaseNestingLevel();
			}
			Object convertedElement = convertIfNecessary(
				indexedPropertyName, null, element, elementType, typeDescriptor);
			if (methodParam != null) {
				methodParam.decreaseNestingLevel();
			}
			
			try {
				convertedCopy.add(convertedElement);
			} catch (Throwable ex) {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format(
						"Collection type [%s] seems to be read-only - injecting original Collection as-is: %s", 
							original.getClass().getName(), ex));
				}
				return original;
			}
			originalAllowed = originalAllowed && (element == convertedElement);
		}
		
		return (originalAllowed ? original : convertedCopy);
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
		MethodParameter methodParam = typeDescriptor.getMethodParameter();
		if (methodParam != null) {
			keyType = GenericCollectionTypeResolver.getMapKeyParameterType(methodParam);
			valType = GenericCollectionTypeResolver.getMapValueParameterType(methodParam);
		}
		if (keyType == null && valType == null && originalAllowed && 
			!propertyEditorRegistry.hasCustomEditorForElement(null, propertyName)) {
			return original;
		}
		
		Iterator<?> it;
		try {
			it = original.entrySet().iterator();
			if (it == null) {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format(
						"Map of type [%s] returned null Iterator - injecting original Map as-is", original.getClass().getName()));
				}
				return original;
			}
		}
		catch (Throwable ex) {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format(
					"Cannot access Map of type [%s] - injecting original Map as-is: %s", original.getClass().getName(), ex));
			}
			return original;
		}
		
		Map<Object, Object> convertedCopy;
		try {
			if (approximable) {
				convertedCopy = CollectionFactory.createApproximateMap(original, original.size());
			}
			else {
				convertedCopy = (Map<Object, Object>) requiredType.newInstance();
			}
		} 
		catch (Throwable ex) {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format(
					"Cannot create copy of Map type [%s] - injecting original Map as-is: %s", 
						original.getClass().getName(), ex));
			}
			return original;
		}
		
		while (it.hasNext()) {
			Entry<?, ?> entry = (Entry<?, ?>) it.next();
			Object key = entry.getKey();
			Object val = entry.getValue();
			String keyedPorpertyName = buildKeyedPropertyName(propertyName, key);
			if (methodParam != null) {
				methodParam.increaseNestingLevel();
				methodParam.setTypeIndexForCurrentLevel(0);
			}
			Object convertedKey = convertIfNecessary(keyedPorpertyName, null, key, keyType, typeDescriptor);
			if (methodParam != null) {
				methodParam.setTypeIndexForCurrentLevel(1);
			}
			Object convertedValue = convertIfNecessary(keyedPorpertyName, null, val, valType, typeDescriptor);
			if (methodParam != null) {
				methodParam.decreaseNestingLevel();
			}
			
			try {
				convertedCopy.put(convertedKey, convertedValue);
			} catch (Throwable ex) {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format(
						"Map type [%s] seems to be read-only - injecting original Map as-is: %s", 
							original.getClass().getName(), ex));
				}
				return original;
			}
			originalAllowed = originalAllowed && (key == convertedKey) && (val == convertedValue);
		}
		
		return (originalAllowed ? original : convertedCopy);
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
