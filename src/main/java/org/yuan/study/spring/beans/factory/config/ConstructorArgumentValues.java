package org.yuan.study.spring.beans.factory.config;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.yuan.study.spring.beans.BeanMetadataElement;
import org.yuan.study.spring.beans.Mergeable;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.ObjectUtils;

public class ConstructorArgumentValues {
	
	private final Map<Integer,ValueHolder> indexedArgumentValues = new HashMap<Integer,ValueHolder>();
	
	private final List<ValueHolder> genericArgumentValues = new LinkedList<ValueHolder>();

	/**
	 * Create new ConstructorArgumentValues
	 */
	public ConstructorArgumentValues() {
	}
	
	/**
	 * Deep copy constructor
	 * @param other
	 */
	public ConstructorArgumentValues(ConstructorArgumentValues other) {
		addArgumentValues(other);
	}

	//------------------------------------------------------------
	// Implementation methods
	//------------------------------------------------------------
	
	/**
	 * Copy all given argument values into this object
	 * @param other
	 */
	public void addArgumentValues(ConstructorArgumentValues other) {
		if (other != null) {
			for (Entry<Integer, ValueHolder> entry : other.indexedArgumentValues.entrySet()) {
				addOrMergeIndexedArgumentValue(entry.getKey(), entry.getValue().copy());
			}
			for (ValueHolder valueHolder : other.genericArgumentValues) {
				if (!genericArgumentValues.contains(valueHolder)) {
					addOrMergeGenericArgumentValue(valueHolder.copy());
				}
			}
		}
	}
	
	/**
	 * Add generic argument value to be matched by type.
	 * @param value
	 */
	public void addGenericArgumentValue(Object value) {
		addGenericArgumentValue(new ValueHolder(value));
	}
	
	/**
	 * Add generic argument value to be matched by type.
	 * @param value
	 * @param type
	 */
	public void addGenericArgumentValue(Object value, String type) {
		addGenericArgumentValue(new ValueHolder(value,type));
	}
	
	/**
	 * Add generic argument value to be matched by type.
	 * @param newValue
	 */
	public void addGenericArgumentValue(ValueHolder newValue) {
		Assert.notNull(newValue, "ValueHolder must not be null");
		if (!genericArgumentValues.contains(newValue)) {
			addOrMergeGenericArgumentValue(newValue);
		}
	}
	
	/**
	 * Add a generic argument value, merging the new value with the current
	 * value if demanded.
	 */
	private void addOrMergeGenericArgumentValue(ValueHolder newValue) {
		if (newValue.getName() != null) {
			for (ValueHolder curValue : genericArgumentValues) {
				if (newValue.getName().equals(curValue.getName()) && newValue.getValue() instanceof Mergeable) {
					Mergeable mergeable = (Mergeable) newValue.getValue();
					if (mergeable.isMergeEnabled()) {
						newValue.setValue(mergeable.merge(curValue.getValue()));
						genericArgumentValues.remove(curValue);
						break;
					}
				}
			}
		}
		genericArgumentValues.add(newValue);
	}
	
	/**
	 * Add argument value for the given index in the constructor argument list.
	 * @param index
	 * @param value
	 */
	public void addIndexedArgumentValue(int index, Object value) {
		addIndexedArgumentValue(index, new ValueHolder(value));
	}
	
	/**
	 * Add argument value for the given index in the constructor argument list.
	 * @param index
	 * @param value
	 * @param type
	 */
	public void addIndexedArgumentValue(int index, Object value, String type) {
		addIndexedArgumentValue(index, new ValueHolder(value, type));
	}
	
	/**
	 * Add an argument value for the given index in the constructor argument list.
	 * @param index
	 * @param newValue
	 */
	public void addIndexedArgumentValue(int index, ValueHolder newValue) {
		Assert.isTrue(index >= 0, "Index must not be negative");
		Assert.notNull(newValue, "ValueHolder must not be null");
		
		addOrMergeIndexedArgumentValue(index, newValue);
	}
	
	/**
	 * Add an argument value for the given index in the constructor argument list,
	 * merging the new value with the current value if demanded.
	 * @param key
	 * @param newValue
	 */
	private void addOrMergeIndexedArgumentValue(Integer key, ValueHolder newValue) {
		ValueHolder curValue = indexedArgumentValues.get(key);
		if (curValue != null && newValue.getValue() instanceof Mergeable) {
			Mergeable mergeable = (Mergeable) newValue.getValue();
			if (mergeable.isMergeEnabled()) {
				newValue.setValue(mergeable.merge(curValue.getValue()));
			}
		}
		indexedArgumentValues.put(key, newValue);
	}
	
	/**
	 * Return the number of argument values held in this instance,
	 * counting both indexed and generic argument values.
	 * @return
	 */
	public int getArgumentCount() {
		return (indexedArgumentValues.size() + genericArgumentValues.size());
	}
	
	/**
	 * Look for an argument value that editor corresponds to the given index 
	 * in the constructor argument list or generically matches by type.
	 * @param index
	 * @param requiredType
	 * @return
	 */
	public ValueHolder getArgumentValue(int index, Class<?> requiredType) {
		return getArgumentValue(index, requiredType, null, null);
	}
	
	/**
	 * Look for an argument value that editor corresponds to the given index 
	 * in the constructor argument list or generically matches by type.
	 * @param index
	 * @param requiredType
	 * @param requiredName
	 * @return
	 */
	public ValueHolder getArgumentValue(int index, Class<?> requiredType, String requiredName) {
		return getArgumentValue(index, requiredType, requiredName, null);
	}
	
	/**
	 * Look for an argument value that editor corresponds to the given index 
	 * in the constructor argument list or generically matches by type.
	 * @param index
	 * @param requiredType
	 * @param usedValueHolders
	 * @return
	 */
	public ValueHolder getArgumentValue(int index, Class<?> requiredType, String requiredName, Set<ValueHolder> usedValueHolders) {
		Assert.isTrue(index >= 0, "Index must not be negative");
		ValueHolder valueHolder = getIndexedArgumentValue(index, requiredType, requiredName);
		if (valueHolder == null) {
			valueHolder = getGenericArgumentValue(requiredType, requiredName, usedValueHolders);
		}
		return valueHolder;
	}
	
	/**
	 * Look for a generic argument value that matches the given type.
	 * @param requiredType
	 * @return
	 */
	public ValueHolder getGenericArgumentValue(Class<?> requiredType) {
		return getGenericArgumentValue(requiredType, null, null);
	}
	
	/**
	 * Look for a generic argument value that matches the given type.
	 * @param requiredType
	 * @param requiredName
	 * @return
	 */
	public ValueHolder getGenericArgumentValue(Class<?> requiredType, String requiredName) {
		return getGenericArgumentValue(requiredType, requiredName, null);
	}
	
	/**
	 * Look for the next generic argument value that matches the given type,
	 * ignoring argument values that have already been used in the current resolution process.
	 * @param requiredType
	 * @param usedValueHolders
	 * @return
	 */
	public ValueHolder getGenericArgumentValue(Class<?> requiredType, String requiredName, Set<ValueHolder> usedValueHolders) {
		for (ValueHolder valueHolder : genericArgumentValues) {
			if (usedValueHolders != null && usedValueHolders.contains(valueHolder)) {
				continue;
			}
			if (valueHolder.getName() != null) {
				if (requiredName == null) {
					continue;
				}
				if (!valueHolder.getName().equals(requiredName)) {
					continue;
				}
				if (!ClassUtils.matchesTypeName(requiredType, valueHolder.getType())) {
					continue;
				}
			}
			if (requiredType != null && valueHolder.getType() == null && valueHolder.getName() == null 
				&& !ClassUtils.isAssignableValue(requiredType, valueHolder.getValue())) {
				continue;
			}
			return valueHolder;
		}
		return null;
	}
	
	/**
	 * Look for a generic argument value that matches the given type.
	 * @return
	 */
	public List<ValueHolder> getGenericArgumentValues() {
		return Collections.unmodifiableList(genericArgumentValues);
	}
	
	/**
	 * 
	 * @param index
	 * @return
	 */
	public boolean hasIndexedArgumentValue(int index) {
		return indexedArgumentValues.containsKey(index);
	}
	
	/**
	 * Get argument value for the given index in teh constructor argument list.
	 * @param index
	 * @param requiredType
	 * @return
	 */
	public ValueHolder getIndexedArgumentValue(int index, Class<?> requiredType) {
		return getIndexedArgumentValue(index, requiredType, null);
	}
	
	/**
	 * Get argument value for the given index in the constructor argument list.
	 * @param index
	 * @param requiredType
	 * @param requiredName
	 * @return
	 */
	public ValueHolder getIndexedArgumentValue(int index, Class<?> requiredType, String requiredName) {
		Assert.isTrue(index >= 0, "Index must not be negative");
		ValueHolder valueHolder = indexedArgumentValues.get(index);
		if (valueHolder != null 
			&& (valueHolder.getType() == null 
				|| (requiredType != null && ClassUtils.matchesTypeName(requiredType, valueHolder.getType()))) 
			&& (valueHolder.getName() == null 
				|| (requiredName != null && requiredName.equals(valueHolder.getName())))) {
			return valueHolder;
		}
		return null;
	}
	
	/**
	 * Return the map of indexed argument values.
	 * @return
	 */
	public Map<Integer,ValueHolder> getIndexedArgumentValues() {
		return Collections.unmodifiableMap(indexedArgumentValues);
	}
	
	/**
	 * Return if this holder does not contain any argument values,
	 * neither indexed ones nor generic ones.
	 * @return
	 */
	public boolean isEmpty() {
		return indexedArgumentValues.isEmpty() 
			&& genericArgumentValues.isEmpty();
	}
	
	/**
	 * Clear this holder, removing all argument values.
	 */
	public void clear() {
		indexedArgumentValues.clear();
		genericArgumentValues.clear();
	}
	
	@Override
	public int hashCode() {
		int result = 7;
		for (ValueHolder valueHolder : genericArgumentValues) {
			result = 31 * result + valueHolder.contentHashCode();
		}
		result = 29 * result;
		for (Entry<Integer, ValueHolder> entry : indexedArgumentValues.entrySet()) {
			result = 31 * result + (entry.getValue().contentHashCode() ^ entry.getKey().hashCode());
		}
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == this) {
			return true;
		}
		if (!(obj instanceof ConstructorArgumentValues)) {
			return false;
		}
		ConstructorArgumentValues other = (ConstructorArgumentValues) obj;
		if (genericArgumentValues.size() != other.genericArgumentValues.size() 
			|| indexedArgumentValues.size() != other.indexedArgumentValues.size()) {
			return false;
		}
		
		Iterator<ValueHolder> it1 = genericArgumentValues.iterator();
		Iterator<ValueHolder> it2 = other.genericArgumentValues.iterator();
		while (it1.hasNext() && it2.hasNext()) {
			ValueHolder vh1 = it1.next();
			ValueHolder vh2 = it2.next();
			if (!vh1.contentEquals(vh2)) {
				return false;
			}
		}
		for (Entry<Integer, ValueHolder> entry : indexedArgumentValues.entrySet()) {
			ValueHolder vh1 = entry.getValue();
			ValueHolder vh2 = other.indexedArgumentValues.get(entry.getKey());
			if (!vh1.contentEquals(vh2)) {
				return false;
			}
		}
		return true;
	}
	
	//------------------------------------------------------------
	// Inner static class 
	//------------------------------------------------------------
	
	public static class ValueHolder implements BeanMetadataElement {
		
		private String name;
		
		private String type;
		
		private Object value;
		
		private Object source;
		
		private boolean converted = false;
		
		private Object convertedValue;
		
		/**
		 * Create a new ValueHolder for the given value and type.
		 * @param value
		 */
		public ValueHolder(Object value) {
			this.value = value;
		}
		
		/**
		 * Create a new ValueHolder for the given value and type.
		 * @param value
		 */
		public ValueHolder(Object value, String type) {
			this.type = type;
			this.value = value;
		}
		
		/**
		 * Create a new ValueHolder for the given value, type and name.
		 * @param value
		 * @param type
		 * @param name
		 */
		public ValueHolder(Object value, String type, String name) {
			this.type = type;
			this.name = name;
			this.value = value;
		}

		/**
		 * Return the type of the constructor argument.
		 * @return
		 */
		public String getType() {
			return type;
		}

		/**
		 * Set the type of the constructor argument.
		 * @param type
		 */
		public void setType(String type) {
			this.type = type;
		}

		/**
		 * Return the value for the constructor argument.
		 * @return
		 */
		public Object getValue() {
			return value;
		}

		/**
		 * Set the value for the constructor argument.
		 * @param value
		 */
		public void setValue(Object value) {
			this.value = value;
		}

		/**
		 * Return the name of the constructor argument.
		 * @return
		 */
		public String getName() {
			return name;
		}

		/**
		 * Set the name of the constructor argument.
		 * @param name
		 */
		public void setName(String name) {
			this.name = name;
		}

		@Override
		public Object getSource() {
			return source;
		}

		/**
		 * Set the configuration source for this metadata element.
		 * @param source
		 */
		public void setSource(Object source) {
			this.source = source;
		}

		/**
		 * Return whether this holder contains a converted value already true,
		 * or whether the value still needs to be converted false.
		 * @return
		 */
		public boolean isConverted() {
			return converted;
		}

		/**
		 * Return the converted value of the constructor argument,
		 * after processed type conversion.
		 * @return
		 */
		public Object getConvertedValue() {
			return convertedValue;
		}

		/**
		 * Set the converted value of the constructor argument,
		 * after processed type conversion.
		 * @param convertedValue
		 */
		public void setConvertedValue(Object convertedValue) {
			this.converted = true;
			this.convertedValue = convertedValue;
		}
		
		/**
		 * Determine whether the content of this ValueHolder is equal 
		 * to the content of the given other ValueHolder.
		 * @param other
		 * @return
		 */
		private boolean contentEquals(ValueHolder other) {
			return (this == other 
				|| (ObjectUtils.nullSafeEquals(value, other.value) 
					&& ObjectUtils.nullSafeEquals(type, other.type)));
		}
		
		/**
		 * Determine whether the hash code of the content of this ValueHolder.
		 * @return
		 */
		private int contentHashCode() {
			return ObjectUtils.nullSafeHashCode(value) * 29 + ObjectUtils.nullSafeHashCode(type);
		}
		
		/**
		 * Create a copy of this ValueHolder: that is, an independent
		 * ValueHolder instance with the same contents.
		 * @return
		 */
		public ValueHolder copy() {
			ValueHolder copy = new ValueHolder(convertedValue, type);
			copy.setSource(this.source);
			return copy;
		}
	}
}
