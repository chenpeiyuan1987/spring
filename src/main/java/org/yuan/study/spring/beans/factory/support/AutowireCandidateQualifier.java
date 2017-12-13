package org.yuan.study.spring.beans.factory.support;

import org.yuan.study.spring.beans.BeanMetadataAttributeAccessor;
import org.yuan.study.spring.util.Assert;

public class AutowireCandidateQualifier extends BeanMetadataAttributeAccessor {
	private static final long serialVersionUID = 1L;

	public static final String VALUE_KEY = "value";
	
	private final String typeName;

	/**
	 * Construct a qualifier to match against an annotation of the
	 * given type.
	 * @param type
	 */
	public AutowireCandidateQualifier(Class<?> type) {
		this(type.getName());
	}
	
	/**
	 * Construct a qualifier to match against an annotation of the
	 * given type.
	 * @param typeName
	 */
	public AutowireCandidateQualifier(String typeName) {
		Assert.notNull(typeName, "Type name must not be null");
		
		this.typeName = typeName;
	}
	
	/**
	 * Construct a qualifier to match against an annotation of the
	 * given type.
	 * @param type
	 * @param value
	 */
	public AutowireCandidateQualifier(Class<?> type, Object value) {
		this(type.getName(), value);
	}
	
	/**
	 * Construct a qualifier to match against an annotation of the
	 * given type.
	 * @param typeName
	 * @param value
	 */
	public AutowireCandidateQualifier(String typeName, Object value) {
		Assert.notNull(typeName, "Type name must not be null");
		
		this.typeName = typeName;
		setAttribute(VALUE_KEY, value);
	}

	/**
	 * Retrieve the type name.
	 * @return
	 */
	public String getTypeName() {
		return typeName;
	}
	
}
