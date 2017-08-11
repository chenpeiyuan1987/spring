package org.yuan.study.spring.beans.factory.support;

import java.util.Map;

import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.ListableBeanFactory;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;

public class DefaultListableBeanFactory 
	extends AbstractAutowireCapableBeanFactory implements ListableBeanFactory, BeanDefinitionRegistry {

	private boolean allowBeanDefinitionOverriding;
	
	
	public DefaultListableBeanFactory() {
		super();
	}

	public DefaultListableBeanFactory(BeanFactory parentBeanFactory) {
		super(parentBeanFactory);
	}


	//-----------------------------------------------------------------
	// Implementation of methods
	//-----------------------------------------------------------------
	
	public boolean isAllowBeanDefinitionOverriding() {
		return allowBeanDefinitionOverriding;
	}

	public void setAllowBeanDefinitionOverriding(
			boolean allowBeanDefinitionOverriding) {
		this.allowBeanDefinitionOverriding = allowBeanDefinitionOverriding;
	}
	
	public void preInstantiateSingletons() {
		
	}
	
	//-----------------------------------------------------------------
	// Implementation of AbstractBeanFactory class
	//-----------------------------------------------------------------
	
	@Override
	public BeanDefinition getBeanDefinition(String beanName)
			throws BeansException {
		// TODO Auto-generated method stub
		return null;
	}
	
	
	//-----------------------------------------------------------------
	// Implementation of AbstractAutowireCapableBeanFactory class
	//-----------------------------------------------------------------
	

	@Override
	protected Map<String, Object> findMatchingBeans(Class<?> requiredType)
		throws BeansException {
		// TODO Auto-generated method stub
		return null;
	}
	
	//-----------------------------------------------------------------
	// Implementation of ListableBeanFactory interface
	//-----------------------------------------------------------------
	
	@Override
	public int getBeanDefinitionCount() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public String[] getBeanDefinitionNames() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String[] getBeanDefinitionNames(Class<?> type) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String[] getBeanNamesForType(Class<?> type) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String[] getBeanNamesForType(Class<?> type,
			boolean includePrototypes, boolean includeFactoryBeans) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Map<String, Object> getBeansOfType(Class<?> type)
			throws BeansException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Map<String, Object> getBeansOfType(Class<?> type,
			boolean includePrototypes, boolean includeFactoryBeans)
			throws BeansException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean containsBeanDefinition(String beanName) {
		// TODO Auto-generated method stub
		return false;
	}
	
	
	//-----------------------------------------------------------------
	// Implementation of BeanDefinitionRegistry interface
	//-----------------------------------------------------------------
	
	@Override
	public void registerBeanDefinition(String beanName,
			BeanDefinition beanDefinition) {
		// TODO Auto-generated method stub
		
	}

	//-----------------------------------------------------------------
	// Implementation of Object class
	//-----------------------------------------------------------------
	
	@Override
	public String toString() {
		// TODO Auto-generated method stub
		return super.toString();
	}
	
}
