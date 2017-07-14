package org.yuan.study.spring.beans.factory.config;

import org.yuan.study.spring.beans.MutablePropertyValues;

public interface BeanDefinition {
	
	ConstructorArgumentValues getConstructorArgumentValues();
	
	MutablePropertyValues getPropertyValues();
	
	String getResourceDescription();
	
	boolean isAbstract();
	
	boolean isLazyInit();
	
	boolean isSingleton();
}
