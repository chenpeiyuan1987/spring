package org.yuan.study.spring.beans.factory.support;

import java.security.AccessControlContext;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.factory.FactoryBean;

public abstract class FactoryBeanRegistrySupport extends DefaultSingletonBeanRegistry {
	
	private final Map<String, Object> factoryBeanObjectCache = new ConcurrentHashMap<String, Object>();

	protected Class<?> getTypeForFactoryBean() {
		
	}
	
	protected Object getCachedObjectForFactoryBean(String beanName) {
		
	}
	
	protected Object getObjectFromFactoryBean(FactoryBean factory, String beanName, boolean shouldPostProcess) {
		
	}
	
	protected Object postProcessObjectFromFactoryBean(Object object, String beanName) throws BeansException {
		
	}
	
	protected FactoryBean getFactoryBean(String beanName, Object beanInstance) throws BeansException {
		
	}
	
	protected void removeSingleton(String beanName) {
		
	}
	
	protected AccessControlContext getAccessControlContext() {
		
	}
	
	private Object doGetObjectFromFactoryBean() {
		
	}
}
