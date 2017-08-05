package org.yuan.study.spring.beans;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyDescriptor;
import java.beans.PropertyEditor;
import java.beans.PropertyEditorManager;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.util.StringUtils;
import org.yuan.study.spring.util.Assert;

public class BeanWrapperImpl extends PropertyEditorRegistrySupport implements BeanWrapper {

	private static final Log logger = LogFactory.getLog(BeanWrapperImpl.class);
	
	private Object object;
	
	private Object rootObject;
	
	private String nestedPath = "";
	
	private boolean extractOldValueForEditor = false;
	
	private Map<String,BeanWrapperImpl> nestedBeanWrappers;
	
	private CachedIntrospectionResults cachedIntrospectionResults;
	
	/**
	 * Create new empty BeanWrapperImpl.
	 */
	public BeanWrapperImpl() {
		this(true);
	}
	
	/**
	 * 
	 */
	public BeanWrapperImpl(boolean registerDefaultEditors) {
		if (registerDefaultEditors) {
			registerDefaultEditors();
		}
	}
	
	/**
	 * 
	 */
	public BeanWrapperImpl(Class<?> clazz) {
		this();
		setWrappedInstance(BeanUtils.instantiateClass(clazz));
	}
	
	/**
	 * 
	 */
	public BeanWrapperImpl(Object object) {
		this();
		setWrappedInstance(object);
	}

	/**
	 * 
	 * @param bean
	 */
	public BeanWrapperImpl(Object object, String nestedPath, Object rootObject) {
		this();
		setWrappedInstance(object, nestedPath, rootObject);
	}
	
	
	//---------------------------------------------------------
	// Implementation methods
	//---------------------------------------------------------
	
	/**
	 * Convert the value to the required type (if necessary from a String).
	 * Conversions from String to any type use the setAsText method of the PropertyEditor class.
	 * @param newValue
	 * @param requiredType
	 * @return
	 * @throws TypeMismatchException
	 */
	public Object doTypeConversionIfNecessary(Object newValue, Class<?> requiredType) throws TypeMismatchException {
		return doTypeConversionIfNecessary(null, null, null, newValue, requiredType);
	}

	/**
	 * 
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
		setIntrospectionClass(object.getClass());
	}
	
	/**
	 * 
	 * @return
	 */
	public Object getRootInstance() {
		return this.rootObject;
	}
	
	/**
	 * 
	 * @return
	 */
	public Class<?> getRootClass() {
		return (this.rootObject != null ? this.rootObject.getClass() : null);
	}
	
	/**
	 * 
	 * @return
	 */
	public String getNestedPath() {
		return this.nestedPath;
	}
	
	/**
	 * Set the class to introspect.
	 * @param clazz
	 */
	protected void setIntrospectionClass(Class<?> clazz) {
		if (this.cachedIntrospectionResults == null 
			|| !this.cachedIntrospectionResults.getBeanClass().equals(clazz)) {
			this.cachedIntrospectionResults = CachedIntrospectionResults.forClass(clazz);
		}
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
	 * 
	 * @param propertyName
	 * @param oldValue
	 * @param newValue
	 * @return
	 */
	private PropertyChangeEvent createPropertyChangeEvent(String propertyName, Object oldValue, Object newValue) {
		// TODO
		return null;
	}
	
	//---------------------------------------------------------
	// Implementation of BeanWrapper interfacee
	//---------------------------------------------------------
	
	@Override
	public Object getWrappedInstance() {
		return this.object;
	}

	@Override
	public void setWrappedInstance(Object object) {
		setWrappedInstance(object, "", null);
	}

	@Override
	public PropertyDescriptor[] getPropertyDescriptors() {
		Assert.state(this.cachedIntrospectionResults != null, "BeanWrapper does not hold a bean instance");
		return this.cachedIntrospectionResults.getBeanInfo().getPropertyDescriptors();
	}

	@Override
	public PropertyDescriptor getPropertyDescriptor(String propertyName) {
		// TODO
		return null;
	}

	@Override
	public Class<?> getWrappedClass() {
		return (this.object != null ? this.object.getClass() : null);
	}

	@Override
	public boolean isExtractOldValueForEditor() {
		return this.extractOldValueForEditor;
	}

	@Override
	public boolean isReadableProperty() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isWritableProperty() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void setExtractOldValueForEditor(boolean extractOldValueForEditor) {
		this.extractOldValueForEditor = extractOldValueForEditor;
	}
	
	@Override
	public Class<?> getPropertyType(String propertyPath) {
		// TODO
		return null;
	}
	
	
	//---------------------------------------------------------
	// Implementation of PropertyAccessor interfacee
	//---------------------------------------------------------
	
	@Override
	public Object getPropertyValue(String propertyName) throws BeansException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setPropertyValue(PropertyValue pv) throws BeansException {
		setPropertyValue(pv.getName(), pv.getValue());
	}

	@Override
	public void setPropertyValue(String propertyName, Object value) throws BeansException {
		
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
	
	
	//-------------------------------------------------------------
	// Inner class for internal use
	//-------------------------------------------------------------
	
	private static class PropertyTokenHolder {
		
		public String canonicalName;
		
		public String actualName;
		
		public String[] keys;
	}
}
