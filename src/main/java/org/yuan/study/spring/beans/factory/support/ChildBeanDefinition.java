package org.yuan.study.spring.beans.factory.support;

import org.yuan.study.spring.beans.MutablePropertyValues;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.beans.factory.config.ConstructorArgumentValues;
import org.yuan.study.spring.util.ObjectUtils;

public class ChildBeanDefinition extends AbstractBeanDefinition {
	private static final long serialVersionUID = 1L;
	
	private String parentName;

	/**
	 * Create a new ChildBeanDefinition for the given parent.
	 * @param parentName
	 */
	public ChildBeanDefinition(String parentName) {
		super();
		this.parentName = parentName;
	}

	/**
	 * Create a new ChildBeanDefinition for the given parent.
	 * @param parentName
	 * @param pvs
	 */
	public ChildBeanDefinition(String parentName, MutablePropertyValues pvs) {
		super(null, pvs);
		this.parentName = parentName;
	}

	/**
	 * Create a new ChildBeanDefinition for the given parent.
	 * @param parentName
	 * @param cargs
	 * @param pvs
	 */
	public ChildBeanDefinition(String parentName, ConstructorArgumentValues cargs, MutablePropertyValues pvs) {
		super(cargs, pvs);
		this.parentName = parentName;
	}
	
	/**
	 * Create a new ChildBeanDefinition for the given parent,
	 * providing  constructor arguments and property values.
	 * @param parentName
	 * @param beanClass
	 * @param cargs
	 * @param pvs
	 */
	public ChildBeanDefinition(String parentName, Class<?> beanClass, ConstructorArgumentValues cargs, MutablePropertyValues pvs) {
		super(cargs, pvs);
		this.parentName = parentName;
		setBeanClass(beanClass);
	}
	
	/**
	 * Create a new ChildBeanDefinition for the given parent,
	 * providing  constructor arguments and property values.
	 * @param parentName
	 * @param beanClassName
	 * @param cargs
	 * @param pvs
	 */
	public ChildBeanDefinition(String parentName, String beanClassName, ConstructorArgumentValues cargs, MutablePropertyValues pvs) {
		super(cargs, pvs);
		this.parentName = parentName;
		setBeanClassName(beanClassName);
	}

	/**
	 * Create a new ChildBeanDefinition for the given parent,
	 * providing  constructor arguments and property values.
	 * @param parentName
	 * @param beanClassName
	 * @param cargs
	 * @param pvs
	 */
	public ChildBeanDefinition(ChildBeanDefinition original) {
		super((BeanDefinition) original);
	}
	
	//---------------------------------------------------------------
	// Implementation of AbstractBeanDefinition class
	//---------------------------------------------------------------
	
	@Override
	public void validate() throws BeanDefinitionValidationException {
		super.validate();
		if (this.parentName == null) {
			throw new BeanDefinitionValidationException("parentName must be set in ChildBeanDefinition");
		}
	}

	@Override
	public void setParentName(String parentName) {
		this.parentName = parentName;
	}
	

	@Override
	public AbstractBeanDefinition cloneBeanDefinition() {
		return new ChildBeanDefinition(this);
	}
	

	@Override
	public int hashCode() {
		return ObjectUtils.nullSafeHashCode(parentName) * 29 + super.hashCode();
	}
	

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof ChildBeanDefinition)) {
			return false;
		}
		ChildBeanDefinition other = (ChildBeanDefinition) obj;
		return ObjectUtils.nullSafeEquals(parentName, other.parentName) && super.equals(obj);
	}
	
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer("Child bean with parent '");
		sb.append(this.parentName).append("': ").append(super.toString());
		return sb.toString();
	}
	
	public String getParentName() {
		return parentName;
	}
}
