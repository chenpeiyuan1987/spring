package org.yuan.study.spring.beans.factory.config;

import org.yuan.study.spring.util.Assert;

public class RuntimeBeanNameReference implements BeanReference {

	private final String beanName;
	
	private Object source;
	
	public RuntimeBeanNameReference(String beanName) {
		Assert.hasText(beanName, "beanName must not be empty");
		this.beanName = beanName;
	}

	@Override
	public Object getSource() {
		return source;
	}

	public void setSource(Object source) {
		this.source = source;
	}

	@Override
	public String getBeanName() {
		return beanName;
	}

	@Override
	public int hashCode() {
		return beanName.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!(obj instanceof RuntimeBeanNameReference))
			return false;
		RuntimeBeanNameReference other = (RuntimeBeanNameReference) obj;
		return beanName.equals(beanName);
	}

	@Override
	public String toString() {
		return "<" + getBeanName() + ">";
	}

}
