package org.yuan.study.spring.beans.factory.config;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.yuan.study.spring.beans.BeanUtils;

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
			this.indexedArgumentValues.putAll(other.indexedArgumentValues);
			this.genericArgumentValues.addAll(other.genericArgumentValues);
		}
	}
	
	/**
	 * Add generic argument value to be matched by type.
	 * @param value
	 */
	public void addGenericArgumentValue(Object value) {
		genericArgumentValues.add(new ValueHolder(value));
	}
	
	/**
	 * Add generic argument value to be matched by type.
	 * @param value
	 * @param type
	 */
	public void addGenericArgumentValue(Object value, String type) {
		genericArgumentValues.add(new ValueHolder(value,type));
	}
	
	/**
	 * Add argument value for the given index in the constructor argument list.
	 * @param index
	 * @param value
	 */
	public void addIndexedArgumentValue(int index, Object value) {
		indexedArgumentValues.put(new Integer(index), new ValueHolder(value));
	}
	
	/**
	 * Add argument value for the given index in the constructor argument list.
	 * @param index
	 * @param value
	 * @param type
	 */
	public void addIndexedArgumentValue(int index, Object value, String type) {
		indexedArgumentValues.put(new Integer(index), new ValueHolder(value, type));
	}
	
	/**
	 * Clear this holder, removing all argument values.
	 */
	public void clear() {
		indexedArgumentValues.clear();
		genericArgumentValues.clear();
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
		return getArgumentValue(index, requiredType, null);
	}
	
	/**
	 * Look for an argument value that editor corresponds to the given index 
	 * in the constructor argument list or generically matches by type.
	 * @param index
	 * @param requiredType
	 * @param usedValueHolders
	 * @return
	 */
	public ValueHolder getArgumentValue(int index, Class<?> requiredType, Set<ValueHolder> usedValueHolders) {
		ValueHolder valueHolder = getIndexedArgumentValue(index, requiredType);
		if (valueHolder == null) {
			valueHolder = getGenericArgumentValue(requiredType, usedValueHolders);
		}
		return valueHolder;
	}
	
	/**
	 * Look for a generic argument value that matches the given type.
	 * @param requiredType
	 * @return
	 */
	public ValueHolder getGenericArgumentValue(Class<?> requiredType) {
		return getGenericArgumentValue(requiredType, null);
	}
	
	/**
	 * Look for the next generic argument value that matches the given type,
	 * ignoring argument values that have already been used in the current resolution process.
	 * @param requiredType
	 * @param usedValueHolders
	 * @return
	 */
	public ValueHolder getGenericArgumentValue(Class<?> requiredType, Set<ValueHolder> usedValueHolders) {
		for (ValueHolder valueHolder : genericArgumentValues) {
			if (usedValueHolders == null || !usedValueHolders.contains(valueHolder)) {
				if (requiredType != null) {
					if (valueHolder.getType() != null) {
						if (valueHolder.getType().equals(requiredType.getName())) {
							return valueHolder;
						}
					}
					else {
						if (BeanUtils.isAssignable(requiredType, valueHolder.getValue())) {
							return valueHolder;
						}
					}
				} 
				else {
					if (valueHolder.getType() == null) {
						return valueHolder;
					}
				}
			}
		}
		return null;
	}
	
	/**
	 * Look for a generic argument value that matches the given type.
	 * @return
	 */
	public List<ValueHolder> getGenericArgumentValues() {
		return genericArgumentValues;
	}
	
	/**
	 * Get argument value for the given index in teh constructor argument list.
	 * @param index
	 * @param requiredType
	 * @return
	 */
	public ValueHolder getIndexedArgumentValue(int index, Class<?> requiredType) {
		ValueHolder valueHolder = indexedArgumentValues.get(new Integer(index));
		if (valueHolder != null) {
			if (valueHolder.getType() == null || requiredType.getName().equals(valueHolder.getType())) {
				return valueHolder;
			}
		}
		return null;
	}
	
	/**
	 * Return the map of indexed argument values.
	 * @return
	 */
	public Map<Integer,ValueHolder> getIndexedArgumentValues() {
		return indexedArgumentValues;
	}
	
	/**
	 * Return if this holder does not contain any argument values,
	 * neither indexed ones nor generic ones.
	 * @return
	 */
	public boolean isEmpty() {
		return (indexedArgumentValues.isEmpty() && genericArgumentValues.isEmpty());
	}
	
	//------------------------------------------------------------
	// Inner static class 
	//------------------------------------------------------------
	
	public static class ValueHolder {
		
		private String type;
		
		private Object value;
		
		public ValueHolder(Object value) {
			this.value = value;
		}
		
		public ValueHolder(Object value, String type) {
			this.value = value;
			this.type = type;
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
		
	}
}
