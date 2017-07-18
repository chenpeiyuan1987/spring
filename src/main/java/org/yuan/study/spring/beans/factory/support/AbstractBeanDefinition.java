package org.yuan.study.spring.beans.factory.support;

import org.yuan.study.spring.beans.MutablePropertyValues;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.beans.factory.config.ConstructorArgumentValues;

public abstract class AbstractBeanDefinition implements BeanDefinition {
	
	public void overrideFrom(AbstractBeanDefinition other) {
		// TODO
	}
	
	public void validate() throws BeanDefinitionValidationException {
		// TODO
	}
	
	public boolean hasBeanClass() {
		// TODO
		return false;
	}
	
	public Class<?> getBeanClass() throws IllegalStateException {
		// TODO
		return null;
	}
	
	public String getFactoryMethodName() {
		// TODO
		return null;
	}
	//--------------------------------------------------------------
	// Implementation of BeanDefinition interface
	//--------------------------------------------------------------
	
	@Override
	public ConstructorArgumentValues getConstructorArgumentValues() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public MutablePropertyValues getPropertyValues() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getResourceDescription() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isAbstract() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isLazyInit() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isSingleton() {
		// TODO Auto-generated method stub
		return false;
	}
}
