package org.yuan.study.spring.core;

import java.io.Serializable;
import java.util.LinkedHashMap;
import java.util.Map;

import org.yuan.study.spring.util.Assert;

public abstract class AttributeAccessorSupport implements AttributeAccessor, Serializable {
	private static final long serialVersionUID = 1L;
	
	private final Map<String, Object> attributes = new LinkedHashMap<String, Object>(0);

	@Override
	public void setAttribute(String name, Object value) {
		Assert.notNull(name, "Name must not be null");
		
		if (value != null) {
			attributes.put(name, value);
		}
		else {
			removeAttribute(name);
		}
	}

	@Override
	public Object getAttribute(String name) {
		Assert.notNull(name, "Name must not be null");
		
		return attributes.get(name);
	}
	
	@Override
	public Object removeAttribute(String name) {
		Assert.notNull(name, "Name must not be null");
		
		return attributes.remove(name);
	}

	@Override
	public boolean hasAttribute(String name) {
		Assert.notNull(name, "Name must not be null");
		
		return attributes.containsKey(name);
	}

	@Override
	public String[] attributeNames() {
		return attributes.keySet().toArray(new String[attributes.size()]);
	}

	/**
	 * Copy the attributes from the supplied AttributeAccessor to this accessor.
	 * @param source
	 */
	protected void copyAttributesFrom(AttributeAccessor source) {
		Assert.notNull(source, "Source must not be null");
		
		String[] attributeNames = source.attributeNames();
		for (String attributeName : attributeNames) {
			setAttribute(attributeName, source.getAttribute(attributeName));
		}
	}

	@Override
	public int hashCode() {
		return attributes.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!(obj instanceof AttributeAccessorSupport)) {
			return false;
		}
		AttributeAccessorSupport other = (AttributeAccessorSupport) obj;
		return attributes.equals(other.attributes);
	}
	
}
