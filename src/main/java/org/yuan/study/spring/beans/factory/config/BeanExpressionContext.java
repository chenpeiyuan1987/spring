package org.yuan.study.spring.beans.factory.config;

import org.yuan.study.spring.util.Assert;

public class BeanExpressionContext {

	private final ConfigurableBeanFactory beanFactory;
	
	private final Scope scope;
	
	public BeanExpressionContext(ConfigurableBeanFactory beanFactory, Scope scope) {
		Assert.notNull(beanFactory, "BeanFactory must not be null");
		
		this.beanFactory = beanFactory;
		this.scope = scope;
	}

	public final ConfigurableBeanFactory getBeanFactory() {
		return beanFactory;
	}

	public final Scope getScope() {
		return scope;
	}
	
	public boolean containsObject(String key) {
		if (beanFactory.containsBean(key)) {
			return true;
		}
		if (scope != null) {
			return scope.resolveContextualObject(key) != null;
		}
		return false;
	}
	
	public Object getObject(String key) {
		if (beanFactory.containsBean(key)) {
			return beanFactory.getBean(key);
		}
		if (scope != null) {
			return scope.resolveContextualObject(key);
		}
		return null;
	}

	@Override
	public int hashCode() {
		return beanFactory.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof BeanExpressionContext)) {
			return false;
		}
		BeanExpressionContext other = (BeanExpressionContext) obj;
		return (beanFactory == other.beanFactory && scope == other.scope);
	}
	
}
