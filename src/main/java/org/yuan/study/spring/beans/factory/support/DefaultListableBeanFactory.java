package org.yuan.study.spring.beans.factory.support;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.util.StringUtils;
import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.ListableBeanFactory;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;

public class DefaultListableBeanFactory 
	extends AbstractAutowireCapableBeanFactory implements ListableBeanFactory, BeanDefinitionRegistry {

	private boolean allowBeanDefinitionOverriding;
	
	private final Map<String,BeanDefinition> beanDefinitionMap = new HashMap<String,BeanDefinition>();
	
	private final List<String> beanDefinitionNames = new ArrayList<String>();
	
	
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

	public void setAllowBeanDefinitionOverriding(boolean allowBeanDefinitionOverriding) {
		this.allowBeanDefinitionOverriding = allowBeanDefinitionOverriding;
	}
	
	public void preInstantiateSingletons() {
		// TODO Auto-generated method stub
	}
	
	private boolean isBeanDefinitionTypeMatch(String beanName, Class<?> type) {
		// TODO Auto-generated method stub
		return false;
	}
	
	private boolean isBeanTypeMatch(String beanName, Class<?> type) {
		// TODO Auto-generated method stub
		return false;
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
		return beanDefinitionMap.size();
	}

	@Override
	public String[] getBeanDefinitionNames() {
		return StringUtils.toStringArray(beanDefinitionNames);
	}

	@Override
	public String[] getBeanDefinitionNames(Class<?> type) {
		// TODO
		return null;
	}

	@Override
	public String[] getBeanNamesForType(Class<?> type) {
		return getBeanNamesForType(type, true, true);
	}

	@Override
	public String[] getBeanNamesForType(Class<?> type,
		boolean includePrototypes, boolean includeFactoryBeans) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Map<String, Object> getBeansOfType(Class<?> type) throws BeansException {
		return getBeansOfType(type, true, true);
	}

	@Override
	public Map<String, Object> getBeansOfType(Class<?> type,
		boolean includePrototypes, boolean includeFactoryBeans) throws BeansException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean containsBeanDefinition(String beanName) {
		return this.beanDefinitionMap.containsKey(beanName);
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
