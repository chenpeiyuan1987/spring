package org.yuan.study.spring.beans.factory.support;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

import org.yuan.study.spring.beans.factory.BeanFactory;

public class SimpleInstantiationStrategy implements InstantiationStrategy {

	@Override
	public Object instantiate(RootBeanDefinition beanDefinition, String beanName, BeanFactory owner) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Object instantiate(RootBeanDefinition beanDefinition, String beanName, BeanFactory owner,
			Constructor<?> ctor, Object[] args) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Object instantiate(RootBeanDefinition beanDefinition, String beanName, BeanFactory owner, Object factoryBean,
			Method factoryMethod, Object[] args) {
		// TODO Auto-generated method stub
		return null;
	}

}
