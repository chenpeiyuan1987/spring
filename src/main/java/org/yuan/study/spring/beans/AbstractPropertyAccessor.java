package org.yuan.study.spring.beans;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public abstract class AbstractPropertyAccessor extends PropertyEditorRegistrySupport implements ConfigurablePropertyAccessor {

	private boolean extractOldValueForEditor = false;

	@Override
	public <T> T convertIfNecessary(Object value, Class<T> requiredType) throws TypeMismatchException {
		return convertIfNecessary(value, requiredType, null);
	}

	@Override
	public void setPropertyValue(PropertyValue pv) throws BeansException {
		setPropertyValue(pv.getName(), pv.getValue());
	}

	@Override
	public void setPropertyValues(Map<?, ?> map) throws BeansException {
		setPropertyValues(new MutablePropertyValues(map));
	}

	@Override
	public void setPropertyValues(PropertyValues pvs) throws BeansException {
		setPropertyValues(pvs, false, false);
	}

	@Override
	public void setPropertyValues(PropertyValues pvs, boolean ignoreUnknown) throws BeansException {
		setPropertyValues(pvs, ignoreUnknown, false);
	}
	
	public void setPropertyValues(PropertyValues pvs, boolean ignoreUnknown, boolean ignoreInvalid) throws BeansException {
		List<PropertyAccessException> propertyAccessExceptions = null;
		List<PropertyValue> propertyValues = (pvs instanceof MutablePropertyValues ? ((MutablePropertyValues) pvs).getPropertyValueList() : Arrays.asList(pvs.getPropertyValues()));
		for (PropertyValue propertyValue : propertyValues) {
			try {
				setPropertyValue(propertyValue);
			} 
			catch (NotWritablePropertyException ex) {
				if (!ignoreUnknown) {
					throw ex;
				}
			}
			catch (NullValueInNestedPathException ex) {
				if (!ignoreUnknown) {
					throw ex;
				}
			}
			catch (PropertyAccessException ex) {
				if (propertyAccessExceptions == null) {
					propertyAccessExceptions = new LinkedList<PropertyAccessException>();
				}
				propertyAccessExceptions.add(ex);
			}
		}
		
	}

	@Override
	public void setExtractOldValueForEditor(boolean extractOldValueForEditor) {
		this.extractOldValueForEditor = extractOldValueForEditor;
	}

	@Override
	public boolean isExtractOldValueForEditor() {
		return this.extractOldValueForEditor;
	}
	
	/**
	 * Readfined with public visibility.
	 */
	public Class<?> getPropertyType(String propertyPath) {
		return null;
	}
	
	/**
	 * Actually get the value of a property.
	 */
	public abstract Object getPropertyValue(String propertyName) throws BeansException;
	
	/**
	 * Actually set a property value.
	 */
	public abstract void setPropertyValue(String propertyName, Object value) throws BeansException;
}
