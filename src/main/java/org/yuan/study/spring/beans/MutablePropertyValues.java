package org.yuan.study.spring.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.yuan.study.spring.util.StringUtils;

public class MutablePropertyValues implements PropertyValues, Serializable {
	private static final long serialVersionUID = 1L;

	private final List<PropertyValue> propertyValueList;
	
	/**
	 * Creates a new empty MutablePropertyValues object.
	 */
	public MutablePropertyValues() {
		this.propertyValueList = new ArrayList<PropertyValue>();
	}
	
	/**
	 * Construct a new PropertyValues object from a Map.
	 * @param original
	 */
	public MutablePropertyValues(Map<String,Object> original) {
		if (original != null) {
			this.propertyValueList = new ArrayList<PropertyValue>(original.size());
			for (Entry<String,Object> entry : original.entrySet()) {
				PropertyValue pv = new PropertyValue(entry.getKey(), entry.getValue());
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
				PropertyValue newPv = new PropertyValue(pv.getName(), pv.getValue());
				this.propertyValueList.add(newPv);
			}
		}
		else {
			this.propertyValueList = new ArrayList<PropertyValue>(0);
		}
	}

	//---------------------------------------------------------
	// Implementation methods
	//---------------------------------------------------------
	
	/**
	 * Add a PropertyValue object, replacing any existing one for the corresponding property.
	 * @param pv
	 * @return
	 */
	public MutablePropertyValues addPropertyValue(PropertyValue pv) {
		for (int i = 0; i < this.propertyValueList.size(); i++) {
			PropertyValue currentPv = (PropertyValue) this.propertyValueList.get(i);
			if (currentPv.getName().equals(pv.getName())) {
				setPropertyValueAt(pv, i);
				return this;
			}
		}
		this.propertyValueList.add(pv);
		return null;
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
	 * Add all property values from the given Map.
	 * @param other
	 * @return
	 */
	public MutablePropertyValues addPropertyValues(Map<String,Object> other) {
		if (other != null) {
			for (Entry<String,Object> entry : other.entrySet()) {
				PropertyValue pv = new PropertyValue(entry.getKey(), entry.getValue());
				addPropertyValue(pv);
			}
		}
		return this;
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
				PropertyValue newPv = new PropertyValue(pv.getName(), pv.getValue());
				addPropertyValue(newPv);
			}
		}
		return this;
	}
	
	/**
	 * Clear thsi holder, removing all PropertyValues.
	 */
	public void clear() {
		this.propertyValueList.clear();
	}
	
	/**
	 * Remove the given PropertyValue, if contained.
	 * @param pv
	 */
	public void removePropertyValue(PropertyValue pv) {
		this.propertyValueList.remove(pv);
	}
	
	/**
	 * Overloaded version of removePropertyValue that takes a property name.
	 * @param propertyName
	 */
	public void removePropertyValue(String propertyName) {
		removePropertyValue(getPropertyValue(propertyName));
	}
	
	/**
	 * Modify a PropertyValue object held in this object.
	 * @param pv
	 * @param i
	 */
	public void setPropertyValueAt(PropertyValue pv, int i) {
		this.propertyValueList.set(i, pv);
	}
	
	public boolean isEmpty() {
		return this.propertyValueList.isEmpty();
	}
	
	
	//---------------------------------------------------------
	// Implementation of PropertyValues interface
	//---------------------------------------------------------
	
	@Override
	public boolean contains(String propertyName) {
		return (getPropertyValue(propertyName) != null);
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
	public PropertyValue getPropertyValue(String propertyName) {
		for (PropertyValue propertyValue : propertyValueList) {
			if (propertyValue.getName().equals(propertyName)) {
				return propertyValue;
			}
		}
		return null;
	}

	@Override
	public PropertyValue[] getPropertyValues() {
		return this.propertyValueList.toArray(new PropertyValue[this.propertyValueList.size()]);
	}

	@Override
	public String toString() {
		PropertyValue[] pvs = getPropertyValues();
		StringBuffer sb = new StringBuffer(String.format("PropertyValues: length=%s; ", pvs.length));
		sb.append(StringUtils.arrayToDelimitedString(pvs, "; "));
		return sb.toString();
	}

}
