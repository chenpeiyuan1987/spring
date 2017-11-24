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
import org.hamcrest.core.IsEqual;
import org.yuan.study.spring.core.CollectionFactory;
import org.yuan.study.spring.core.GenericCollectionTypeResolver;
import org.yuan.study.spring.core.MethodParameter;
import org.yuan.study.spring.core.convert.ConversionException;
import org.yuan.study.spring.core.convert.ConverterNotFoundException;
import org.yuan.study.spring.core.convert.TypeDescriptor;
import org.yuan.study.spring.core.convert.support.PropertyTypeDescriptor;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.StringUtils;

public class BeanWrapperImpl extends AbstractPropertyAccessor implements BeanWrapper {

	private static final Log logger = LogFactory.getLog(BeanWrapperImpl.class);
	
	/** The wrapped object */
	private Object object;
	
	private String nestedPath = "";
	
	private Object rootObject;
	
	private TypeConverterDelegate typeConverterDelegate;
	
	/**
	 * The security context used for invoking the property methods.
	 */
	private AccessControlContext acc;
	
	/** 
	 * Cached introspections results for this object, 
	 * to prevent encountering the cost of JavaBeans introspection every time. 
	 */
	private CachedIntrospectionResults cachedIntrospectionResults;
	
	/** Map with cached nested BeanWrappers: nested path -> BeanWrapper instance. */
	private Map<String,BeanWrapperImpl> nestedBeanWrappers;
	
	private boolean autoGrowNestedPaths = false;
	
	private int autoGrowCollectionLimit = Integer.MAX_VALUE;
	
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
	// Implementation of BeanWrapper interface
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
		return (object != null ? object.getClass() : null);
	}
	
	/**
	 * Return the nested path of the object wrapped by this BeanWrapper.
	 * @return
	 */
	public final String getNestedPath() {
		return nestedPath;
	}
	
	/**
	 * Return the root object at the top of the path of this BeanWrapper.
	 * @return
	 */
	public final Object getRootInstance() {
		return rootObject;
	}
	
	/**
	 * Return the class of the root object at the top of the path of this BeanWrapper.
	 * @return
	 */
	public final Class<?> getRootClass() {
		return (rootObject != null ? rootObject.getClass() : null);
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
		if (cachedIntrospectionResults != null 
			&& !clazz.equals(cachedIntrospectionResults.getBeanClass())) {
			cachedIntrospectionResults = null;
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
					return new PropertyTypeDescriptor(pd, new MethodParameter(pd.getReadMethod(), -1), type);
				}
				else if (pd.getWriteMethod() != null) {
					return new PropertyTypeDescriptor(pd, BeanUtils.getWriteMethodParameter(pd), type);
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

	@Override
	public <T> T convertIfNecessary(Object value, Class<T> requiredType, MethodParameter methodParam) throws TypeMismatchException {
		try {
			return typeConverterDelegate.convertIfNecessary(value, requiredType, methodParam);
		}
		catch (ConverterNotFoundException ex) {
			throw new ConversionNotSupportedException(value, requiredType, ex);
		}
		catch (ConversionException ex) {
			throw new TypeMismatchException(value, requiredType, ex);
		}
		catch (IllegalStateException ex) {
			throw new ConversionNotSupportedException(value, requiredType, ex);
		}
		catch (IllegalArgumentException ex) {
			throw new TypeMismatchException(value, requiredType, ex);
		}
	}
	
	public Object convertIfNecessary(String propertyName, Object oldValue, Object newValue, Class<?> requiredType, TypeDescriptor td) throws TypeMismatchException {
		try {
			return typeConverterDelegate.convertIfNecessary(propertyName, oldValue, newValue, requiredType, td);
		} 
		catch (ConverterNotFoundException ex) {
			PropertyChangeEvent pce = new PropertyChangeEvent(rootObject, nestedPath + propertyName, oldValue, newValue);
			throw new ConversionNotSupportedException(pce, td.getType(), ex);
		}
		catch (ConversionException ex) {
			PropertyChangeEvent pce = new PropertyChangeEvent(rootObject, nestedPath + propertyName, oldValue, newValue);
			throw new TypeMismatchException(pce, requiredType, ex);
		}
		catch (IllegalStateException ex) {
			PropertyChangeEvent pce = new PropertyChangeEvent(rootObject, nestedPath + propertyName, oldValue, newValue);
			throw new ConversionNotSupportedException(pce, requiredType, ex);
		}
		catch (IllegalArgumentException ex) {
			PropertyChangeEvent pce = new PropertyChangeEvent(rootObject, nestedPath + propertyName, oldValue, newValue);
			throw new TypeMismatchException(pce, requiredType, ex);
		}
		
	}
	
	private Object convertIfNecessary(String properName, Object oldValue, Object newValue, Class<?> requiredType) throws TypeMismatchException {
		return convertIfNecessary(properName, oldValue, newValue, requiredType, TypeDescriptor.valueOf(requiredType));
	}
	
	/**
	 * Convert the given value for the specified property to the latter's type.
	 * @param value
	 * @param propertyName
	 * @return
	 * @throws TypeMismatchException
	 */
	public Object convertForProperty(Object value, String propertyName) throws TypeMismatchException {
		PropertyDescriptor pd = getCachedIntrospectionResults().getPropertyDescriptor(propertyName);
		if (pd == null) {
			throw new InvalidPropertyException(getRootClass(), nestedPath + propertyName, String.format("No property '%s' found", propertyName));
		}
		return convertForProperty(propertyName, null, value, pd);
	}
	
	private Object convertForProperty(String propertyName, Object oldValue, Object newValue, PropertyDescriptor pd) throws TypeMismatchException {
		return convertIfNecessary(propertyName, oldValue, newValue, pd.getPropertyType(), new PropertyTypeDescriptor(pd, BeanUtils.getWriteMethodParameter(pd)));
	}
	
	//---------------------------------------------------------
	// Implementation methods
	//---------------------------------------------------------
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
			if (autoGrowNestedPaths) {
				propertyValue = setDefaultValue(tokens);
			} 
			else {
				throw new NullValueInNestedPathException(getRootClass(), this.nestedPath + cannoicalName);
			}
			
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
	
	private Object setDefaultValue(String propertyName) {
		PropertyTokenHolder tokens = new PropertyTokenHolder();
		tokens.actualName = propertyName;
		tokens.canonicalName = propertyName;
		return setDefaultValue(tokens);
	}
	
	private Object setDefaultValue(PropertyTokenHolder tokens) {
		PropertyValue pv = createDefaultPropertyValue(tokens);
		setPropertyValue(tokens, pv);
		return getPropertyValue(tokens);
	}
	
	private PropertyValue createDefaultPropertyValue(PropertyTokenHolder tokens) {
		Class<?> type = getPropertyType(tokens.canonicalName);
		if (type == null) {
			throw new NullValueInNestedPathException(getRootClass(), this.nestedPath + tokens.canonicalName, 
				"Could not determine property type for auto-growing a fefault value");
		}
		Object defaultValue = newValue(type, tokens.canonicalName);
		return new PropertyValue(tokens.canonicalName, defaultValue);
	}
	
	private Object newValue(Class<?> type, String name) {
		try {
			if (type.isArray()) {
				Class<?> componentType = type.getComponentType();
				if (componentType.isArray()) {
					Object array = Array.newInstance(componentType, 1);
					Array.set(array, 0, Array.newInstance(componentType.getComponentType(), 0));
					return array;
				}
				else {
					return Array.newInstance(componentType, 0);
				}
			}
			if (Collection.class.isAssignableFrom(type)) {
				return CollectionFactory.createCollection(type, 16);
			}
			if (Map.class.isAssignableFrom(type)) {
				return CollectionFactory.createMap(type, 16);
			}
			return type.newInstance();
		} 
		catch (Exception ex) {
			throw new NullValueInNestedPathException(getRootClass(), this.nestedPath + name, 
				"Could not instantiate property type [" + type.getName() + "] to auto-growing nested property path: " + ex);
		}
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
	
	//---------------------------------------------------------
	// Implementation of PropertyAccessor interfacee
	//---------------------------------------------------------
	
	
	//---------------------------------------------------------------------
	// Implementation of PropertyAccessor interface
	//---------------------------------------------------------------------
	
	@Override
	public Object getPropertyValue(String propertyName) throws BeansException {
		BeanWrapperImpl nestedBw = getBeanWrapperForPropertyPath(propertyName);
		PropertyTokenHolder tokens = getPropertyNameTokens(getFinalPath(nestedBw, propertyName));
		return nestedBw.getPropertyValue(tokens);
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

	private Object growArrayIfNecessary(Object array, int index, String name) {
		if (!autoGrowNestedPaths) {
			return array;
		}
		
		int length = Array.getLength(array);
		if (index >= length && index < autoGrowCollectionLimit) {
			Class<?> componentType = array.getClass().getComponentType();
			Object newArray = Array.newInstance(componentType, index + 1);
			System.arraycopy(array, 0, newArray, 0, length);
			for (int i = length; i < Array.getLength(newArray); i++) {
				Array.set(newArray, i, newValue(componentType, name));
			}
			setPropertyValue(name, newArray);
			return getPropertyValue(name);
		} 
		else {
			return array;
		}
	}
	
	private void growCollectionIfNecessary(Collection collection, int index, String name, PropertyDescriptor pd, int nestingLevel) {
		if (!autoGrowNestedPaths) {
			return;
		}
		
		int size = collection.size();
		if (index >= size && index < autoGrowCollectionLimit) {
			Class<?> elementType = GenericCollectionTypeResolver.getCollectionReturnType(pd.getReadMethod(), nestingLevel);
			if (elementType != null) {
				for (int i = collection.size(); i < index + 1; i++) {
					collection.add(newValue(elementType, name));
				}
			}
		}
	}

	
	@Override
	public void setPropertyValue(String propertyName, Object value) throws BeansException {
		BeanWrapperImpl nestedBw = null;
		try {
			nestedBw = getBeanWrapperForPropertyPath(propertyName);
		} 
		catch (NotReadablePropertyException ex) {
			throw new NotWritablePropertyException(getRootClass(), this.nestedPath + propertyName, 
				String.format("Nested property in path '%s' does not exist", propertyName), ex);
		}
		PropertyTokenHolder tokens = getPropertyNameTokens(getFinalPath(nestedBw, propertyName));
		nestedBw.setPropertyValue(tokens, new PropertyValue(propertyName, value));
	}
	
	
	@Override
	public void setPropertyValue(PropertyValue pv) throws BeansException {
		PropertyTokenHolder tokens = (PropertyTokenHolder) pv.resolvedTokens;
		if (tokens == null) {
			String propertyName = pv.getName();
			BeanWrapperImpl nestedBw;
			try {
				nestedBw = getBeanWrapperForPropertyPath(propertyName);
			}
			catch (NotReadablePropertyException ex) {
				throw new NotWritablePropertyException(getRootClass(), this.nestedPath + propertyName, 
					String.format("Nested property in path '%s' does not exist", propertyName), ex);
			}
			tokens = getPropertyNameTokens(getFinalPath(nestedBw, propertyName));
			if (nestedBw == this) {
				pv.getOriginalPropertyValue().resolvedTokens = tokens;
			}
			nestedBw.setPropertyValue(tokens, pv);
		}
		else {
			setPropertyValue(tokens, pv);
		}
	}
	
	
	private void setPropertyValue(PropertyTokenHolder tokens, PropertyValue pv) throws BeansException {
		String propertyName = tokens.canonicalName;
		String actualName = tokens.actualName;
		
		if (tokens.keys != null) {
			PropertyTokenHolder getterTokens = new PropertyTokenHolder();
			getterTokens.canonicalName = tokens.canonicalName;
			getterTokens.actualName = tokens.actualName;
			getterTokens.keys = new String[tokens.keys.length - 1];
			System.arraycopy(tokens.keys, 0, getterTokens.keys, 0, tokens.keys.length - 1);
			Object propValue;
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
			if (propValue.getClass().isArray()) {
				PropertyDescriptor pd = getCachedIntrospectionResults().getPropertyDescriptor(actualName);
				Class<?> requiredType = propValue.getClass().getComponentType();
				int arrayIndex = Integer.parseInt(key);
				Object oldValue = null;
				try {
					if (arrayIndex < Array.getLength(propValue) && ) {
						oldValue = Array.get(propValue, arrayIndex);
					}
					Object convertedValue = convertIfNecessary(propertyName, oldValue, pv.getValue(), requiredType, 
						new PropertyTypeDescriptor(pd, new MethodParameter(pd.getReadMethod(), -1), requiredType));
					Array.set(propValue, arrayIndex, convertedValue);
				} 
				catch (IndexOutOfBoundsException e) {
					// TODO: handle exception
				}
			}
			else if (propValue instanceof List) {
				
			}
			else if (propValue instanceof Map) {
				
			}
			else {
				
			}
		} 
		else {
			PropertyDescriptor pd = pv.resolvedDescriptor;
			if (pd == null || !pd.getWriteMethod().getDeclaringClass().isInstance(this.object)) {
				
			}
			
			Object oldValue = null;
			try {
				
			} 
			catch (TypeMismatchException ex) {
				throw ex;
			}
			catch (InvocationTargetException ex) {
				
			}
			catch (Exception ex) {
				
			}
		}
		
	}
	
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(getClass().getName());
		if (this.object != null) {
			sb.append(": wrapping object for type [");
			sb.append(this.object.getClass().getName()).append("]");
		} 
		else {
			sb.append(": no wrapped object set");
		}
		return sb.toString();
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
