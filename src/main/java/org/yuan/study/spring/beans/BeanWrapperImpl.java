package org.yuan.study.spring.beans;

import java.beans.PropertyDescriptor;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
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
	 * 
	 * @param newValue
	 * @param requiredType
	 * @return
	 * @throws TypeMismatchException
	 */
	public Object doTypeConversionIfNecessary(Object newValue, Class<?> requiredType) throws TypeMismatchException {
		return null;
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
	
}
