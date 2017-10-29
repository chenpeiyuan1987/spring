package org.yuan.study.spring.beans;

import java.util.Map;

public abstract class AbstractPropertyAccessor extends PropertyEditorRegistrySupport implements ConfigurablePropertyAccessor {

	private boolean extractOldValueForEditor = false;

	@Override
	public <T> T convertIfNecessary(Object value, Class<T> requiredType) throws TypeMismatchException {
		// TODO Auto-generated method stub
		return null;
	}

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
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setPropertyValues(Map<?, ?> map) throws BeansException {
		// TODO Auto-generated method stub
		
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
		
	}

	@Override
	public void setExtractOldValueForEditor(boolean extractOldValueForEditor) {
		this.extractOldValueForEditor = extractOldValueForEditor;
	}

	@Override
	public boolean isExtractOldValueForEditor() {
		return this.extractOldValueForEditor;
	}
	
	
}
