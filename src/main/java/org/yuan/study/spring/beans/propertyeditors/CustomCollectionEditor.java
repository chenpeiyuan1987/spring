package org.yuan.study.spring.beans.propertyeditors;

import java.awt.List;
import java.beans.PropertyEditorSupport;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;

import org.yuan.study.spring.core.CollectionFactory;


public class CustomCollectionEditor extends PropertyEditorSupport {

	private final Class<?> collectionType;
	
	private final boolean nullAsEmptyCollection;
	
	/**
	 * Create a new CustomCollectionEditor for the given target type,
	 * keeping an incoming null as-is.
	 * @param collectionType
	 */
	public CustomCollectionEditor(Class<?> collectionType) {
		this(collectionType, false);
	}
	
	/**
	 * Create a new CustomCollectionEditor for the given target type.
	 * @param collectionType
	 * @param nullAsEmptyCollection
	 */
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
	
	/**
	 * Create a Collection of the given type, with the given initial capacity 
	 * (if supported by the Collection type).
	 * @param collectionType
	 * @param initialCapacity
	 * @return
	 */
	protected Collection<?> createCollection(Class<?> collectionType, int initialCapacity) {
		if (!collectionType.isInterface()) {
			try {
				return (Collection<?>) collectionType.newInstance();
			}
			catch (Exception ex) {
				throw new IllegalArgumentException(String.format("Could not instantiate collection class [%s]: %s", collectionType.getName(), ex.getMessage()));
			}
		}
		if (List.class.equals(collectionType)) {
			return new ArrayList<Object>(initialCapacity);
		}
		if (SortedSet.class.equals(collectionType)) {
			return new TreeSet<Object>();
		}
		return CollectionFactory.createLinkedSetIfPossible(initialCapacity);
	}

	/**
	 * Hook to convert each encountered Collection/array element.
	 * @param element
	 * @return
	 */
	protected Object convertElement(Object element) {
		return element;
	}
	
	/**
	 * Return whether to always create a new Collection,
	 * even if the type of the passed-in Collection already matches.
	 * @return
	 */
	protected boolean alwaysCreateNewCollection() {
		return false;
	}
	
	@Override
	public void setValue(Object value) {
		if (value == null) {
			if (nullAsEmptyCollection) {
				super.setValue(createCollection(this.collectionType, 0));
				return;
			}
			super.setValue(value);
			return;
		} 
		else {
			if (collectionType.isInstance(value) && !alwaysCreateNewCollection()) {
				super.setValue(value);
				return;
			}
			if (value instanceof Collection) {
				Collection<Object> source = (Collection<Object>) value;
				Collection<Object> target = (Collection<Object>)createCollection(collectionType, source.size());
				for (Object object : source) {
					target.add(convertElement(object));
				}
				super.setValue(target);
				return;
			}
			if (value.getClass().isArray()) {
				int length = Array.getLength(value);
				Collection<Object> target = (Collection<Object>)createCollection(collectionType, length);
				for (int i = 0; i < length; i++) {
					target.add(Array.get(value, i));
				}
				super.setValue(target);
				return;
			}
			Collection<Object> target = (Collection<Object>)createCollection(collectionType, 1);
			target.add(convertElement(value));
			super.setValue(target);
			return;
		}
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
