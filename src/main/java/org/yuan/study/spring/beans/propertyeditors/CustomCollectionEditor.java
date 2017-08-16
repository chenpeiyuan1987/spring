package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;
import java.util.Collection;

import org.hamcrest.core.IsAnything;

public class CustomCollectionEditor extends PropertyEditorSupport {

	private final Class<?> collectionType;
	
	private final boolean nullAsEmptyCollection;
	

	public CustomCollectionEditor(Class<?> collectionType) {
		this(collectionType, false);
	}
	
	public CustomCollectionEditor(Class<?> collectionType, boolean nullAsEmptyCollection) {
		if (collectionType == null) {
			throw new IllegalArgumentException("Collection type is required");
		}
		if (!Collection.class.isAssignableFrom(collectionType)) {
			throw new IllegalArgumentException(
				String.format("Collection type [%s] does not implement [java.util.Collection]", collectionType.getName()));
		}
		this.collectionType = collectionType;
		this.nullAsEmptyCollection = nullAsEmptyCollection;
	}

	
	@Override
	public void setValue(Object value) {
		// TODO Auto-generated method stub
		super.setValue(value);
	}

	@Override
	public String getAsText() {
		return null;
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		setValue(text);
	}
	
}
