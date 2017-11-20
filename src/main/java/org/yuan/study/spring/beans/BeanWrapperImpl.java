package org.yuan.study.spring.beans;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyDescriptor;
import java.beans.PropertyEditor;
import java.beans.PropertyEditorManager;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.security.AccessControlContext;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.yuan.study.spring.core.MethodParameter;
import org.yuan.study.spring.core.convert.TypeDescriptor;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.StringUtils;

public class BeanWrapperImpl extends PropertyEditorRegistrySupport implements BeanWrapper {

	private static final Log logger = LogFactory.getLog(BeanWrapperImpl.class);
	
	/** The wrapped object */
	private Object object;
	
	private Object rootObject;
	
	private String nestedPath = "";
	
	private TypeConverterDelegate typeConverterDelegate;
	
	/** Map with cached nested BeanWrappers: nested path -> BeanWrapper instance. */
	private Map<String,BeanWrapperImpl> nestedBeanWrappers;
	
	private boolean autoGrowNestedPaths = false;
	
	private int autoGrowCollectionLimit = Integer.MAX_VALUE;
	
	/** 
	 * Cached introspections results for this object, 
	 * to prevent encountering the cost of JavaBeans introspection every time. 
	 */
	private CachedIntrospectionResults cachedIntrospectionResults;
	
	/**
	 * The security context used for invoking the property methods.
	 */
	private AccessControlContext acc;
	
	
	/**
	 * Create new empty BeanWrapperImpl. Wrapped instance needs to be set afterwards.
	 */
	public BeanWrapperImpl() {
		this(true);
	}
	
	/**
	 * Create new empty BeanWrapperImpl. Wrapped instance needs to be set afterwards.
	 */
	public BeanWrapperImpl(boolean registerDefaultEditors) {
		if (registerDefaultEditors) {
			registerDefaultEditors();
		}
		typeConverterDelegate = new TypeConverterDelegate(this);
	}
	
	/**
	 * Create new BeanWrapperImpl, wrapping a new instance of the specified class.
	 */
	public BeanWrapperImpl(Class<?> clazz) {
		registerDefaultEditors();
		setWrappedInstance(BeanUtils.instantiateClass(clazz));
	}
	
	/**
	 * Create new BeanWrapperImpl for the given object.
	 */
	public BeanWrapperImpl(Object object) {
		registerDefaultEditors();
		setWrappedInstance(object);
	}

	/**
	 * Create new BeanWrapperImpl for the given object,
	 * registering a nested path that the object is in.
	 * @param bean
	 */
	public BeanWrapperImpl(Object object, String nestedPath, Object rootObject) {
		this();
		setWrappedInstance(object, nestedPath, rootObject);
	}
	
	/**
	 * Create new BeanWrapperImpl for the given object,
	 * registering a nested path that the object is in.
	 * @param object
	 * @param nestedPath
	 * @param superBw
	 */
	private BeanWrapperImpl(Object object, String nestedPath, BeanWrapperImpl superBw) {
		setWrappedInstance(object, nestedPath, superBw.getWrappedInstance());
		setExtractOldValueForEditor(superBw.isExtractOldValueForEditor());
		setAutoGrowNestedPaths(superBw.isAutoGrowNestedPaths());
		setAutoGrowCollectionLimit(superBw.getAutoGrowCollectionLimit());
		setConversionService(superBw.getConversionService());
		setSecurityContext(superBw.acc);
	}
	
	//---------------------------------------------------------
	// Implementation methods
	//---------------------------------------------------------
	
	/**
	 * Switch the target object, replacing the cached introsspection results only
	 * if the class of the new object is different to that of the replaced object.
	 * @param object
	 */
	public void setWrappedInstance(Object object) {
		setWrappedInstance(object, "", null);
	}
	
	/**
	 * Switch the target object, replacing the cached introspection results only 
	 * if the class of the new object is different to that of the replaced object.
	 * @param object
	 * @param nestedPath
	 * @param rootObject
	 */
	public void setWrappedInstance(Object object, String nestedPath, Object rootObject) {
		Assert.notNull(object, "Bean object must not be null");
		
		this.object = object;
		this.nestedPath = (nestedPath != null ? nestedPath : "");
		this.rootObject = (!"".equals(this.nestedPath) ? rootObject : object);
		this.nestedBeanWrappers = null;
		this.typeConverterDelegate = new TypeConverterDelegate(this, object);
		setIntrospectionClass(object.getClass());
	}
	
	@Override
	public final Object getWrappedInstance() {
		return this.object;
	}
	
	@Override
	public final Class<?> getWrappedClass() {
		return (this.object != null ? this.object.getClass() : null);
	}
	
	/**
	 * Return the nested path of the object wrapped by this BeanWrapper.
	 * @return
	 */
	public final String getNestedPath() {
		return this.nestedPath;
	}
	
	/**
	 * Return the root object at the top of the path of this BeanWrapper.
	 * @return
	 */
	public final Object getRootInstance() {
		return this.rootObject;
	}
	
	/**
	 * Return the class of the root object at the top of the path of this BeanWrapper.
	 * @return
	 */
	public final Class<?> getRootClass() {
		return (this.rootObject != null ? this.rootObject.getClass() : null);
	}
	
	@Override
	public void setAutoGrowNestedPaths(boolean autoGrowNestedPaths) {
		this.autoGrowNestedPaths = autoGrowNestedPaths;
	}

	@Override
	public boolean isAutoGrowNestedPaths() {
		return autoGrowNestedPaths;
	}

	@Override
	public void setAutoGrowCollectionLimit(int autoGrowCollectionLimit) {
		this.autoGrowCollectionLimit = autoGrowCollectionLimit;
	}

	@Override
	public int getAutoGrowCollectionLimit() {
		return autoGrowCollectionLimit;
	}
	
	/**
	 * Set the security context used during the invocation of the wrapped instance methods.
	 * @param acc
	 */
	public void setSecurityContext(AccessControlContext acc) {
		this.acc = acc;
	}
	
	/**
	 * Return the security context used during the invocation of the wrapped instance methods.
	 * @return
	 */
	public AccessControlContext getSecurityContext() {
		return acc;
	}
	
	/**
	 * Set the class to introspect.
	 * @param clazz
	 */
	protected void setIntrospectionClass(Class<?> clazz) {
		if (this.cachedIntrospectionResults != null 
			&& !clazz.equals(this.cachedIntrospectionResults.getBeanClass())) {
			this.cachedIntrospectionResults = null;
		}
	}
	
	/**
	 * Obtain a lazily initializted CachedIntrospectionResults instance
	 * for the wrapped object.
	 * @return
	 */
	private CachedIntrospectionResults getCachedIntrospectionResults() {
		Assert.state(object != null, "BeanWrapper does not hold a bean instance");
		if (cachedIntrospectionResults == null) {
			cachedIntrospectionResults = CachedIntrospectionResults.forClass(getWrappedClass());
		}
		return cachedIntrospectionResults;
	}
	
	@Override
	public PropertyDescriptor[] getPropertyDescriptors() {
		return getCachedIntrospectionResults().getPropertyDescriptors();
	}

	@Override
	public PropertyDescriptor getPropertyDescriptor(String propertyName) {
		PropertyDescriptor pd = getPropertyDescriptorInternal(propertyName);
		if (pd == null) {
			throw new InvalidPropertyException(getRootClass(), this.nestedPath + propertyName, 
				String.format("No property '%s' found", propertyName));
		}
		return pd;
	}
	
	/**
	 * Internal version of getPropertyDescriptor: 
	 * Returns null if not found rather than throwing an exception.
	 * @param propertyName
	 * @return
	 * @throws BeansException
	 */
	protected PropertyDescriptor getPropertyDescriptorInternal(String propertyName) throws BeansException {
		Assert.notNull(propertyName, "Property name must not be null");
		BeanWrapperImpl nestedBw = getBeanWrapperForPropertyPath(propertyName);
		return nestedBw.getCachedIntrospectionResults().getPropertyDescriptor(getFinalPath(nestedBw, propertyName));
	}
	
	@Override
	public Class<?> getPropertyType(String propertyName) {
		try {
			PropertyDescriptor pd = getPropertyDescriptorInternal(propertyName);
			if (pd != null) {
				return pd.getPropertyType();
			}
			else {
				Object value = getPropertyValue(propertyName);
				if (value != null) {
					return value.getClass();
				}
				
				Class<?> editorType = guessPropertyTypeFromEditors(propertyName);
				if (editorType != null) {
					return editorType;
				}
			}
		} 
		catch (InvalidPropertyException ex) {
		}
		return null;
	}
	
	@Override
	public TypeDescriptor getPropertyTypeDescriptor(String propertyName) throws BeansException {
		try {
			String actualPropertyName = PropertyAccessorUtils.getPropertyName(propertyName);
			PropertyDescriptor pd = getPropertyDescriptorInternal(actualPropertyName);
			if (pd != null) {
				Class<?> type = getPropertyType(propertyName);
				if (pd.getReadMethod() != null) {
					
				}
				else if (pd.getWriteMethod() != null) {
					return new Property
				}
			}
		} catch (InvalidPropertyException ex) {}
		return null;
	}
	
	@Override
	public boolean isReadableProperty(String propertyName) {
		try {
			PropertyDescriptor pd = getPropertyDescriptorInternal(propertyName);
			if (pd != null) {
				if (pd.getReadMethod() != null) {
					return true;
				}
			}
			else {
				getPropertyValue(propertyName);
				return true;
			}
		}
		catch (InvalidPropertyException ex) {
			
		}
		return false;
	}

	@Override
	public boolean isWritableProperty(String propertyName) {
		try {
			PropertyDescriptor pd = getPropertyDescriptorInternal(propertyName);
			if (pd != null) {
				if (pd.getWriteMethod() != null) {
					return true;
				}
			}
			else {
				getPropertyValue(propertyName);
				return true;
			}
		}
		catch (InvalidPropertyException ex) {
			
		}
		return false;
	}

	public Object convertForProperty(Object value, String propertyName) throws TypeMismatchException {
		PropertyDescriptor pd = getCachedIntrospectionResults().getPropertyDescriptor(propertyName);
		if (pd == null) {
			throw new InvalidPropertyException(getRootClass(), nestedPath + propertyName, String.format("No property '%s' found", propertyName));
		}
		return convertForProperty(propertyName, null, value, pd);
	}
	
	private Object convertForProperty(String propertyName, Object oldValue, Object newValue, PropertyDescriptor pd) throws TypeMismatchException {
		
	}
	
	/**
	 * Convert the value to the required type (if necessary from a String), for the specified property.
	 * @param propertyName
	 * @param fullPropertyName
	 * @param oldValue
	 * @param newValue
	 * @param requiredType
	 * @return
	 * @throws TypeMismatchException
	 */
	protected Object doTypeConversionIfNecessary(String propertyName, String fullPropertyName, 
		Object oldValue, Object newValue, Class<?> requiredType) throws TypeMismatchException {
		Object convertedValue = newValue;
		
		PropertyEditor propertyEditor = findCustomEditor(requiredType, fullPropertyName);
		
		if (propertyEditor != null 
			|| (requiredType != null 
				&& (requiredType.isArray() || !requiredType.isInstance(convertedValue)))) {
			
			if (requiredType != null) {
				if (propertyEditor == null) {
					propertyEditor = getDefaultEditor(requiredType);
					if (propertyEditor == null) {
						propertyEditor = PropertyEditorManager.findEditor(requiredType);
					}
				}
			}
			
			if (propertyEditor != null && !(convertedValue instanceof String)) {
				try {
					propertyEditor.setValue(convertedValue);
					Object newConvertedValue = propertyEditor.getValue();
					if (newConvertedValue != convertedValue) {
						convertedValue = newConvertedValue;
						propertyEditor = null;
					}
				} 
				catch (IllegalArgumentException ex) {
					throw new TypeMismatchException(
						createPropertyChangeEvent(fullPropertyName, oldValue, newValue), requiredType, ex);
				}
			}
			
			if (requiredType != null && !requiredType.isArray() && convertedValue instanceof String[]) {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Converting String array to comma-delimited String [%s]", convertedValue));
				}
				convertedValue = StringUtils.arrayToCommaDelimitedString((String[])convertedValue);
			}
			
			if (propertyEditor != null && convertedValue instanceof String) {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Converting String to [%s] using property editor [%s]", requiredType, propertyEditor));
				}
				try {
					propertyEditor.setValue(oldValue);
					propertyEditor.setAsText((String) convertedValue);
					convertedValue = propertyEditor.getValue();
				}
				catch (IllegalArgumentException ex) {
					throw new TypeMismatchException(
							createPropertyChangeEvent(fullPropertyName, oldValue, newValue), requiredType, ex);
				}
			}
			
			if (requiredType != null) {
				if (requiredType.isArray()) {
					Class<?> componentType = requiredType.getComponentType();
					if (convertedValue instanceof Collection) {
						Collection<?> collection = (Collection<?>) convertedValue;
						Object result = Array.newInstance(componentType, collection.size());
						Iterator<?> iterator = collection.iterator();
						for (int i = 0; iterator.hasNext(); i++) {
							Object value = doTypeConversionIfNecessary(propertyName, 
								propertyName + PROPERTY_KEY_PREFIX + i + PROPERTY_KEY_SUFFIX, 
								null, iterator.next(), componentType);
							Array.set(result, i, value);
						}
						return result;
					}
					else if (convertedValue != null && convertedValue.getClass().isArray()) {
						int length = Array.getLength(convertedValue);
						Object result = Array.newInstance(componentType, length);
						for (int i = 0; i < length; i++) {
							Object value = doTypeConversionIfNecessary(propertyName, 
								propertyName + PROPERTY_KEY_PREFIX + i + PROPERTY_KEY_SUFFIX, 
								null, Array.get(result, i), componentType);
							Array.set(result, i, value);
						}
						return result;
					}
					else if (convertedValue != null) {
						Object result = Array.newInstance(componentType, 1);
						Object value = doTypeConversionIfNecessary(propertyName, 
							propertyName + PROPERTY_KEY_PREFIX + 0 + PROPERTY_KEY_SUFFIX, 
							null, convertedValue, componentType);
						Array.set(result, 0, value);
						return result;
					}
				}
				
				if (convertedValue != null && !requiredType.isPrimitive() 
					&& !requiredType.isInstance(convertedValue)) {
					
					if (convertedValue instanceof String) {
						try {
							Field enumField = requiredType.getField((String)convertedValue);
							return enumField.get(null);
						} 
						catch (Exception ex) {
							if (logger.isDebugEnabled()) {
								logger.debug(String.format("Field [%s] isn't an enum value", convertedValue), ex);
							}
						}
					}
					
					throw new TypeMismatchException(
						createPropertyChangeEvent(fullPropertyName, oldValue, newValue), requiredType);
				}
			}
		}
		
		return convertedValue;
	}
	
	/**
	 * Create a new nested BeanWrapper instance.
	 * @param object
	 * @param nestedPath
	 * @return
	 */
	protected BeanWrapperImpl newNestedBeanWrapper(Object object, String nestedPath) {
		return new BeanWrapperImpl(object, nestedPath, this);
	}
	
	/**
	 * Recursively navigate to return a BeanWrapper for the nested property path.
	 * @param propertyPath
	 * @return
	 */
	protected BeanWrapperImpl getBeanWrapperForPropertyPath(String propertyPath) {
		int pos = PropertyAccessorUtils.getFirstNestedPropertySeparatorIndex(propertyPath);
		if (pos > -1) {
			String nestedProperty = propertyPath.substring(0, pos);
			String nestedPath = propertyPath.substring(pos + 1);
			BeanWrapperImpl nestedBw = getNestedBeanWrapper(nestedProperty);
			return nestedBw.getBeanWrapperForPropertyPath(nestedPath);
		} 
		else {
			return this;
		}
	}
	
	/**
	 * Get the last component of the path. Also works if not nested.
	 * @param bw
	 * @param nestedPath
	 * @return
	 */
	private String getFinalPath(BeanWrapper bw, String nestedPath) {
		if (bw == this) {
			return nestedPath;
		}
		return nestedPath.substring(PropertyAccessorUtils.getLastNestedPropertySeparatorIndex(nestedPath) + 1);
	}
	
	/**
	 * Retrieve a BeanWrapper for the given nested property.
	 * Create a new one if not found in the cache.
	 * @param nestedProperty
	 * @return
	 */
	private BeanWrapperImpl getNestedBeanWrapper(String nestedProperty) {
		if (this.nestedBeanWrappers == null) {
			this.nestedBeanWrappers = new HashMap<String,BeanWrapperImpl>();
		}
		
		PropertyTokenHolder tokens = getPropertyNameTokens(nestedProperty);
		String cannoicalName = tokens.canonicalName;
		Object propertyValue = getPropertyValue(tokens);
		if (propertyValue == null) {
			throw new NullValueInNestedPathException(getRootClass(), this.nestedPath + cannoicalName);
		}
		
		BeanWrapperImpl nestedBw = (BeanWrapperImpl) this.nestedBeanWrappers.get(cannoicalName);
		if (nestedBw == null || nestedBw.getWrappedInstance() != propertyValue) {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("Creating new nested BeanWrapper for property '%s'", cannoicalName));
			}
			nestedBw = newNestedBeanWrapper(propertyValue, this.nestedPath + cannoicalName + NESTED_PROPERTY_SEPARATOR);
			copyDefaultEditorsTo(nestedBw);
			copyCustomEditorsTo(nestedBw, cannoicalName);
			this.nestedBeanWrappers.put(cannoicalName, nestedBw);
		} else {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("Using cached nested BeanWrapper for property '%s'", cannoicalName));
			}
		}
		
		return nestedBw;
	}
	
	/**
	 * Create a new PropertyChangeEvent
	 * @param propertyName
	 * @param oldValue
	 * @param newValue
	 * @return
	 */
	private PropertyChangeEvent createPropertyChangeEvent(String propertyName, Object oldValue, Object newValue) {
		return new PropertyChangeEvent((this.rootObject != null ? this.rootObject : "constructor"), 
			(propertyName != null ? this.nestedPath + propertyName : null), oldValue, newValue);
	}
	
	/**
	 * Get property value
	 * @param tokens
	 * @return
	 * @throws BeansException
	 */
	private Object getPropertyValue(PropertyTokenHolder tokens) throws BeansException {
		String propertyName = tokens.canonicalName;
		String actualName = tokens.actualName;
		PropertyDescriptor propertyDescriptor = getPropertyDescriptorInternal(tokens.actualName);
		if (propertyDescriptor == null || propertyDescriptor.getReadMethod() == null) {
			throw new NotReadablePropertyException(getRootClass(), this.nestedPath + propertyName);
		}
		Method readMethod = propertyDescriptor.getReadMethod();
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("About to invoke read method [%s] on object of class [%s]", readMethod, this.object.getClass().getName()));
		}
		try {
			if (!Modifier.isPrivate(readMethod.getDeclaringClass().getModifiers())) {
				readMethod.setAccessible(true);
			}
			Object value = readMethod.invoke(this.object, (Object[]) null);
			if (tokens.keys != null) {
				for (String  key : tokens.keys) {
					if (value == null) {
						throw new NullValueInNestedPathException(getRootClass(), this.nestedPath + propertyName, 
							String.format("Cannot access indexed value of property referenced in indexed property path '%s': returned null", propertyName));
					}
					else if (value.getClass().isArray()) {
						value = Array.get(value, Integer.parseInt(key));
					}
					else if (value instanceof List) {
						List<?> list = (List<?>) value;
						value = list.get(Integer.parseInt(key));
					} 
					else if (value instanceof Set) {
						Set<?> set = (Set<?>) value;
						int index = Integer.parseInt(key);
						if (index < 0 || index >= set.size()) {
							throw new InvalidPropertyException(getRootClass(), this.nestedPath + propertyName, 
								String.format("Cannot get element with index %s from Set of size %s, accessed using property path '%s'", index, set.size(), propertyName));
						}
						int i = 0;
						for (Object elem : set) {
							if (i == index) {
								value = elem;
								break;
							}
							i++;
						}
					}
					else if (value instanceof Map) {
						Map<?,?> map = (Map<?,?>) value;
						value = map.get(key);
					}
					else {
						throw new InvalidPropertyException(getRootClass(), this.nestedPath + propertyName, 
							String.format("Property referenced in indexed property path '%s' is neither an array nor a List nor a Set nor a Map; returned value was [%s]", propertyName, value));
					}
				}
			}
			return value;
		} 
		catch (InvocationTargetException ex) {
			throw new InvalidPropertyException(getRootClass(), this.nestedPath + propertyName, 
				String.format("Getter for property '%s' threw exception", actualName), ex);
		}
		catch (IllegalAccessException ex) {
			throw new InvalidPropertyException(getRootClass(), this.nestedPath + propertyName, 
					String.format("Illegal attempt to get property '%s' threw exception", actualName), ex);
		}
		catch (IndexOutOfBoundsException ex) {
			throw new InvalidPropertyException(getRootClass(), this.nestedPath + propertyName, 
					String.format("Index of out of bounds in property path '%s'", propertyName), ex);
		}
		catch (NumberFormatException ex) {
			throw new InvalidPropertyException(getRootClass(), this.nestedPath + propertyName, 
					String.format("Invalid index in property path '%s'", propertyName), ex);
		}
	}
	
	/**
	 * Parse the given property name into the corresponding property name tokens.
	 * @param propertyName
	 * @return
	 */
	private PropertyTokenHolder getPropertyNameTokens(String propertyName) {
		PropertyTokenHolder tokens = new PropertyTokenHolder();
		String actualName = null;
		List<String> keys = new ArrayList<String>(2);
		int searchIndex = 0;
		while (searchIndex != -1) {
			int keyStart = propertyName.indexOf(PROPERTY_KEY_PREFIX, searchIndex);
			searchIndex = -1;
			if (keyStart != -1) {
				int keyEnd = propertyName.indexOf(PROPERTY_KEY_SUFFIX, keyStart + PROPERTY_KEY_PREFIX.length());
				if (keyEnd != -1) {
					if (actualName == null) {
						actualName = propertyName.substring(0, keyStart);
					}
					String key = propertyName.substring(keyStart + PROPERTY_KEY_PREFIX.length(), keyEnd);
					if ((key.startsWith("'") && key.endsWith("'")) || (key.startsWith("\"") && key.endsWith("\""))) {
						key = key.substring(1, key.length() - 1);
					}
					keys.add(key);
					searchIndex = keyEnd + PROPERTY_KEY_SUFFIX.length();
				}
			}
		}
		tokens.actualName = (actualName != null ? actualName : propertyName);
		tokens.canonicalName = tokens.actualName;
		if (!keys.isEmpty()) {
			tokens.canonicalName += 
				PROPERTY_KEY_PREFIX + 
				StringUtils.collectionToDelimitedString(keys, PROPERTY_KEY_SUFFIX + PROPERTY_KEY_PREFIX) + 
				PROPERTY_KEY_SUFFIX;
			tokens.keys = StringUtils.toStringArray(keys);
		}
		return tokens;
	}
	
	private void setPropertyValue(PropertyTokenHolder tokens, Object newValue) throws BeansException {
		String propertyName = tokens.canonicalName;
		
		if (tokens.keys != null) {
			PropertyTokenHolder getterTokens = new PropertyTokenHolder();
			getterTokens.canonicalName = tokens.canonicalName;
			getterTokens.actualName = tokens.actualName;
			getterTokens.keys = new String[tokens.keys.length - 1];
			System.arraycopy(tokens.keys, 0, getterTokens.keys, 0, tokens.keys.length - 1);
			Object propValue = null;
			try {
				propValue = getPropertyValue(getterTokens);
			}
			catch (NotReadablePropertyException ex) {
				throw new NotWritablePropertyException(getRootClass(), this.nestedPath + propertyName, 
					String.format("Cannot access indexed value in property referenced in indexed property path '%s'", propertyName), ex);
			}
			
			String key = tokens.keys[tokens.keys.length - 1];
			if (propValue == null) {
				throw new NullValueInNestedPathException(getRootClass(), this.nestedPath + propertyName, 
					String.format("Cannot access indexed value in property referenced in indexed property path '%s': returned null", propertyName));
			}
			else if (propValue.getClass().isArray()) {
				Class<?> requiredType = propValue.getClass().getComponentType();
				int index = Integer.parseInt(key);
				Object oldValue = null;
				try {
					if (isExtractOldValueForEditor()) {
						oldValue = Array.get(propValue, index);
					}
					Object convertedValue = doTypeConversionIfNecessary(propertyName, propertyName, oldValue, newValue, requiredType);
					Array.set(propValue, index, convertedValue);
				}
				catch (IllegalArgumentException ex) {
					PropertyChangeEvent pce = new PropertyChangeEvent(this.rootObject, this.nestedPath + propertyName, oldValue, newValue);
					throw new TypeMismatchException(pce, requiredType, ex);
				}
				catch (IndexOutOfBoundsException ex) {
					throw new InvalidPropertyException(getRootClass(), this.nestedPath + propertyName, 
						String.format("Invalid array index in property path '%s'", propertyName), ex);
				}
			} 
			else if(propValue instanceof List) {
				List<Object> list = (List<Object>) propValue;
				int index = Integer.parseInt(key);
				Object oldValue = null;
				if (isExtractOldValueForEditor() && index < list.size()) {
					oldValue = list.get(index);
				}
				Object convertedValue = doTypeConversionIfNecessary(propertyName, propertyName, oldValue, newValue, null);
				if (index < list.size()) {
					list.set(index, convertedValue);
				}
				else {
					for (int i = list.size(); i < index; i++) {
						try {
							list.add(null);
						}
						catch (NullPointerException ex) {
							throw new InvalidPropertyException(getRootClass(), this.nestedPath + propertyName, 
								String.format("Cannot set element with index %s in List of size %s, accessed using property path %s: List does not support filling up gaps with null elements", index, list.size(), propertyName));
						}
					}
					list.add(convertedValue);
				}
			}
			else if(propValue instanceof Map) {
				Map<String,Object> map = (Map<String,Object>) propValue;
				Object oldValue = null;
				if (isExtractOldValueForEditor()) {
					oldValue = map.get(key);
				}
				Object convertedValue = doTypeConversionIfNecessary(propertyName, propertyName, oldValue, newValue, null);
				map.put(key, convertedValue);
			}
			else {
				throw new InvalidPropertyException(getRootClass(), this.nestedPath + propertyName, 
					String.format("Property referenced in indexed property path '%s' is neither an array nor a List nor a Map; returned value was [%s]", propertyName, newValue));
			}
		}
		
		else {
			PropertyDescriptor pd = getPropertyDescriptorInternal(propertyName);
			if (pd == null || pd.getWriteMethod() == null) {
				throw new NotWritablePropertyException(getRootClass(), this.nestedPath + propertyName);
			}
			
			Method readMethod = pd.getReadMethod();
			Method writeMethod = pd.getWriteMethod();
			Object oldValue = null;
			
			if (isExtractOldValueForEditor() && readMethod != null) {
				if (!Modifier.isPublic(readMethod.getDeclaringClass().getModifiers())) {
					readMethod.setAccessible(true);
				}
				try {
					oldValue = readMethod.invoke(this.object, new Object[0]);
				}
				catch (Exception ex) {
					if (logger.isDebugEnabled()) {
						logger.debug(String.format("Could not read previous value of property '%s'", this.nestedPath + propertyName), ex);
					}
				}
			}
			
			try {
				Object convertedValue = doTypeConversionIfNecessary(propertyName, propertyName, oldValue, newValue, pd.getPropertyType());
				if (pd.getPropertyType().isPrimitive() && (convertedValue == null || "".equals(convertedValue))) {
					throw new IllegalArgumentException(
						String.format("Invalid value [%s] for property '%s' of primitive type [%s]", 
							newValue, pd.getName(), pd.getPropertyType()));
				}
				
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("About to invoke write method [%s] on object of class [%s]", writeMethod, this.object.getClass().getName()));
				}
				if (!Modifier.isPublic(writeMethod.getDeclaringClass().getModifiers())) {
					writeMethod.setAccessible(true);
				}
				writeMethod.invoke(this.object, new Object[]{convertedValue});
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Invoked write method [%s] with value of type [%s]", writeMethod, pd.getPropertyType().getName()));
				}
			}
			catch (InvocationTargetException ex) {
				PropertyChangeEvent propertyChangeEvent = new PropertyChangeEvent(this.rootObject, this.nestedPath + propertyName, oldValue, newValue);
				if (ex.getTargetException() instanceof ClassCastException) {
					throw new TypeMismatchException(propertyChangeEvent, pd.getPropertyType(), ex.getTargetException());
				}
				else {
					throw new MethodInvocationException(propertyChangeEvent, ex.getTargetException());
				}
			}
			catch (IllegalArgumentException ex) {
				PropertyChangeEvent propertyChangeEvent = new PropertyChangeEvent(this.rootObject, this.nestedPath + propertyName, oldValue, newValue);
				throw new TypeMismatchException(propertyChangeEvent, pd.getPropertyType(), ex);
			}
			catch (IllegalAccessException ex) {
				PropertyChangeEvent propertyChangeEvent = new PropertyChangeEvent(this.rootObject, this.nestedPath + propertyName, oldValue, newValue);
				throw new MethodInvocationException(propertyChangeEvent, ex);
			}
		}
	}
	
	
	//---------------------------------------------------------
	// Implementation of BeanWrapper interfacee
	//---------------------------------------------------------

	
	//---------------------------------------------------------
	// Implementation of PropertyAccessor interfacee
	//---------------------------------------------------------
	
	@Override
	public Object getPropertyValue(String propertyName) throws BeansException {
		BeanWrapperImpl nestedBw = getBeanWrapperForPropertyPath(propertyName);
		PropertyTokenHolder tokens = getPropertyNameTokens(getFinalPath(nestedBw, propertyName));
		return nestedBw.getPropertyValue(tokens);
	}

	@Override
	public void setPropertyValue(PropertyValue pv) throws BeansException {
		setPropertyValue(pv.getName(), pv.getValue());
	}

	@Override
	public void setPropertyValue(String propertyName, Object value) throws BeansException {
		BeanWrapperImpl nestedBw = null;
		try {
			nestedBw = getBeanWrapperForPropertyPath(propertyName);
		} catch (NotReadablePropertyException ex) {
			throw new NotWritablePropertyException(getRootClass(), this.nestedPath + propertyName, String.format("Nested property in path '%s' does not exist", propertyName), ex);
		}
		PropertyTokenHolder tokens = getPropertyNameTokens(getFinalPath(nestedBw, propertyName));
		nestedBw.setPropertyValue(tokens, value);
	}
	
	@Override
	public void setPropertyValues(Map<String, Object> map) throws BeansException {
		setPropertyValues(new MutablePropertyValues(map));
	}

	@Override
	public void setPropertyValues(PropertyValues pvs) throws BeansException {
		setPropertyValues(pvs, false);
	}

	@Override
	public void setPropertyValues(PropertyValues pvs, boolean ignoreUnknown) throws BeansException {
		List<PropertyAccessException> propertyAccessExceptions = new ArrayList<PropertyAccessException>();
		for (PropertyValue pv : pvs.getPropertyValues()) {
			try {
				setPropertyValue(pv);
			}
			catch (NotWritablePropertyException ex) {
				if (!ignoreUnknown) {
					throw ex;
				}
			}
			catch (PropertyAccessException ex) {
				propertyAccessExceptions.add(ex);
			}
		}
		
		if (!propertyAccessExceptions.isEmpty()) {
			PropertyAccessException[] paeArray = propertyAccessExceptions.toArray(new PropertyAccessException[propertyAccessExceptions.size()]);
			throw new PropertyAccessExceptionsException(this, paeArray);
		}
	}

	//------------------------------------------------------------
	// Implementation of other methods
	//------------------------------------------------------------
	
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer(getClass().getName());
		if (this.object != null) {
			sb.append(": wrapping object for type [")
				.append(this.object.getClass().getName()).append("]");
		} 
		else {
			sb.append(": no wrapped object set");
		}
		return sb.toString();
	}
	
	@Override
	public void setPropertyValues(PropertyValues pvs, boolean ignoreUnknown,
			boolean ignoreInvalid) throws BeansException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public <T> T convertIfNecessary(Object value, Class<T> requiredType) throws TypeMismatchException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public <T> T convertIfNecessary(Object value, Class<T> requiredType, MethodParameter methodParam) throws TypeMismatchException {
		try {
			return typeConverterDelegate.
		}
		catch (ConverterNotFoundException ex) {
		}
		
		return null;
	}

	//-------------------------------------------------------------
	// Inner class for internal use
	//-------------------------------------------------------------
	
	private static class PropertyTokenHolder {
		
		public String canonicalName;
		
		public String actualName;
		
		public String[] keys;
	}

}
