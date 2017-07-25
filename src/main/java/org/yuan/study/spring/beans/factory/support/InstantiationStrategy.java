package org.yuan.study.spring.beans.factory.support;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

import org.yuan.study.spring.beans.factory.BeanFactory;

public interface InstantiationStrategy {

	/**
	 * 
	 * @param beanDefinition
	 * @param beanName
	 * @param owner
	 * @return
	 */
	Object instantiate(RootBeanDefinition beanDefinition, String beanName, BeanFactory owner);
	
	/**
	 * 
	 * @param beanDefinition
	 * @param beanName
	 * @param owner
	 * @param ctor
	 * @param args
	 * @return
	 */
	Object instantiate(RootBeanDefinition beanDefinition, String beanName, BeanFactory owner, Constructor<?> ctor, Object[] args);

	/**
	 * 
	 * @param beanDefinition
	 * @param beanName
	 * @param owner
	 * @param factoryBean
	 * @param factoryMethod
	 * @param args
	 * @return
	 */
	Object instantiate(RootBeanDefinition beanDefinition, String beanName, BeanFactory owner, Object factoryBean, Method factoryMethod, Object[] args);
}
