package org.yuan.study.spring.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.yuan.study.spring.util.StringUtils;

public class MutablePropertyValues implements PropertyValues, Serializable {
	private static final long serialVersionUID = 1L;

	private final List<PropertyValue> propertyValueList;
	
	private Set<String> processedProperties;
	
	private volatile boolean converted = false;
	
	/**
	 * Creates a new empty MutablePropertyValues object.
	 */
	public MutablePropertyValues() {
		this.propertyValueList = new ArrayList<PropertyValue>(0);
	}
	
	/**
	 * Construct a new PropertyValues object from a Map.
	 * @param original
	 */
	public MutablePropertyValues(Map<?, ?> original) {
		if (original != null) {
			this.propertyValueList = new ArrayList<PropertyValue>(original.size());
			for (Entry<?, ?> entry : original.entrySet()) {
				PropertyValue pv = new PropertyValue(entry.getKey().toString(), entry.getValue());
				this.propertyValueList.add(pv);
			}
		}
		else {
			this.propertyValueList = new ArrayList<PropertyValue>(0);
		}
	}

	/**
	 * Deep copy constructor.
	 * @param original
	 */
	public MutablePropertyValues(PropertyValues original) {
		if (original != null) {
			PropertyValue[] pvs = original.getPropertyValues();
			this.propertyValueList = new ArrayList<PropertyValue>(pvs.length);
			for (PropertyValue pv : pvs) {
				this.propertyValueList.add(new PropertyValue(pv));
			}
		}
		else {
			this.propertyValueList = new ArrayList<PropertyValue>(0);
		}
	}

	/**
	 * Construct a new MutablePropertyValues object using the given List 
	 * of PropertyValue objects as-is.
	 * @param propertyValueList
	 */
	public MutablePropertyValues(List<PropertyValue> propertyValueList) {
		this.propertyValueList = (propertyValueList != null ? propertyValueList : new ArrayList<PropertyValue>());
	}
	
	//---------------------------------------------------------
	// Implementation methods
	//---------------------------------------------------------
	
	/**
	 * Return the underlying List of PropertyValue objects in its raw form.
	 * The returned List can be modified directly, although this is not recommended.
	 * @return
	 */
	public List<PropertyValue> getPropertyValueList() {
		return propertyValueList;
	}
	
	/**
	 * Return the number of PropertyValue entries in the list.
	 * @return
	 */
	public int size() {
		return propertyValueList.size();
	}
	
	/**
	 * Copy all given PropertyValues into this object.
	 * @param other
	 * @return
	 */
	public MutablePropertyValues addPropertyValues(PropertyValues other) {
		if (other != null) {
			PropertyValue[] pvs = other.getPropertyValues();
			for (PropertyValue pv : pvs) {
				addPropertyValue(new PropertyValue(pv));
			}
		}
		return this;
	}
	
	/**
	 * Add all property values from the given Map.
	 * @param other
	 * @return
	 */
	public MutablePropertyValues addPropertyValues(Map<?, ?> other) {
		if (other != null) {
			for (Entry<?, ?> entry : other.entrySet()) {
				PropertyValue pv = new PropertyValue(entry.getKey().toString(), entry.getValue());
				addPropertyValue(pv);
			}
		}
		return this;
	}
	
	/**
	 * Add a PropertyValue object, replacing any existing one for the corresponding property.
	 * @param pv
	 * @return
	 */
	public MutablePropertyValues addPropertyValue(PropertyValue pv) {
		for (int i = 0; i < this.propertyValueList.size(); i++) {
			PropertyValue currentPv = (PropertyValue) this.propertyValueList.get(i);
			if (currentPv.getName().equals(pv.getName())) {
				pv = mergeIfRequired(pv, currentPv);
				setPropertyValueAt(pv, i);
				return this;
			}
		}
		this.propertyValueList.add(pv);
		return this;
	}
	
	/**
	 * Overloaded version of addPropertyValue that takes a property name and a property value.
	 * @param propertyName
	 * @param propertyValue
	 */
	public void addPropertyValue(String propertyName, Object propertyValue) {
		addPropertyValue(new PropertyValue(propertyName, propertyValue));
	}
	
	/**
	 * Add a PropertyValue object, replacing any existing one for the 
	 * corresponding property or getting merged with it.
	 * @param propertyName
	 * @param propertyValue
	 * @return
	 */
	public MutablePropertyValues add(String propertyName, Object propertyValue) {
		addPropertyValue(new PropertyValue(propertyName, propertyValue));
		return this;
	}
	
	/**
	 * Modify a PropertyValue object held in this object.
	 * @param pv
	 * @param i
	 */
	public void setPropertyValueAt(PropertyValue pv, int i) {
		propertyValueList.set(i, pv);
	}
	
	/**
	 * Merges the value of the supplied 'new' PropertyValue with that of
	 * the current PropertyValue if merging is supported and enabled.
	 * @param newPv
	 * @param currentPv
	 * @return
	 */
	private PropertyValue mergeIfRequired(PropertyValue newPv, PropertyValue currentPv) {
		Object value = newPv.getValue();
		if (value instanceof Mergeable) {
			Mergeable mergeable = (Mergeable) value;
			if (mergeable.isMergeEnabled()) {
				Object merged = mergeable.merge(currentPv.getValue());
				return new PropertyValue(newPv.getName(), merged);
			}
		}
		return newPv;
	}
	
	/**
	 * Remove the given PropertyValue, if contained.
	 * @param pv
	 */
	public void removePropertyValue(PropertyValue pv) {
		propertyValueList.remove(pv);
	}
	
	/**
	 * Overloaded version of removePropertyValue that takes a property name.
	 * @param propertyName
	 */
	public void removePropertyValue(String propertyName) {
		propertyValueList.remove(getPropertyValue(propertyName));
	}
	
	//---------------------------------------------------------
	// Implementation of PropertyValues interface
	//---------------------------------------------------------

	@Override
	public PropertyValue[] getPropertyValues() {
		return propertyValueList.toArray(new PropertyValue[propertyValueList.size()]);
	}
	
	@Override
	public PropertyValue getPropertyValue(String propertyName) {
		for (PropertyValue propertyValue : propertyValueList) {
			if (propertyValue.getName().equals(propertyName)) {
				return propertyValue;
			}
		}
		return null;
	}

	@Override
	public PropertyValues changesSince(PropertyValues old) {
		MutablePropertyValues changes = new MutablePropertyValues();
		if (old == this) {
			return changes;
		}
		for (PropertyValue newPv : propertyValueList) {
			PropertyValue oldPv = old.getPropertyValue(newPv.getName());
			if (oldPv == null) {
				changes.addPropertyValue(newPv);
			}
			else if (!oldPv.equals(newPv)) {
				changes.addPropertyValue(newPv);
			}
		}
		return changes;
	}
	
	@Override
	public boolean contains(String propertyName) {
		return (getPropertyValue(propertyName) != null || 
			(processedProperties != null && processedProperties.contains(propertyName)));
	}
	
	public boolean isEmpty() {
		return propertyValueList.isEmpty();
	}
	
	/**
	 * Register the specified property as "processed" in the sense
	 * of some processor calling the corresponding setter method
	 * putside of the PropertyValues mechanism.
	 * @param propertyName
	 */
	public void registerProcessedProperty(String propertyName) {
		if (processedProperties == null) {
			processedProperties = new HashSet<String>();
		}
		processedProperties.add(propertyName);
	}
	
	public boolean isConverted() {
		return converted;
	}

	public void setConverted() {
		this.converted = true;
	}

	@Override
	public int hashCode() {
		return propertyValueList.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!(obj instanceof MutablePropertyValues))
			return false;
		MutablePropertyValues other = (MutablePropertyValues) obj;
		return propertyValueList.equals(other.propertyValueList);
	}

	@Override
	public String toString() {
		PropertyValue[] pvs = getPropertyValues();
		StringBuilder sb = new StringBuilder();
		sb.append("PropertyValues: length=").append(pvs.length);
		if (pvs.length > 0) {
			sb.append(";").append(StringUtils.arrayToDelimitedString(pvs, "; "));
		}
		return sb.toString();
	}

}
