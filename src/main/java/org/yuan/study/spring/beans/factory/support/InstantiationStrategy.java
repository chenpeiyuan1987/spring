package org.yuan.study.spring.beans.factory.support;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

import org.yuan.study.spring.beans.factory.BeanFactory;

public interface InstantiationStrategy {

	/**
	 * Return an instance of the bean with the given name in this factory.
	 * @param beanDefinition
	 * @param beanName
	 * @param owner
	 * @return
	 */
	Object instantiate(RootBeanDefinition beanDefinition, String beanName, BeanFactory owner);
	
	/**
	 * Return an instance of the bean with the given name in this factory, creating it via the given constructor.
	 * @param beanDefinition
	 * @param beanName
	 * @param owner
	 * @param ctor
	 * @param args
	 * @return
	 */
	Object instantiate(RootBeanDefinition beanDefinition, String beanName, BeanFactory owner, Constructor<?> ctor, Object[] args);

	/**
	 * Return an instance of the bean with the given name in this factory, creating it via the given factory method.
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
