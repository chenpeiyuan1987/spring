package org.yuan.study.spring.beans.factory.support;

import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.factory.BeanCreationException;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.config.AutowireCapableBeanFactory;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;

public abstract class AbstractAutowireCapableBeanFactory 
	extends AbstractBeanFactory implements AutowireCapableBeanFactory {
	
	
	
	public AbstractAutowireCapableBeanFactory() {
		super();
		// TODO Auto-generated constructor stub
	}

	public AbstractAutowireCapableBeanFactory(BeanFactory parentBeanFactory) {
		super(parentBeanFactory);
		// TODO Auto-generated constructor stub
	}

	//-----------------------------------------------------------------
	// Implementation of AutowireCapableBeanFactory interface
	//-----------------------------------------------------------------
	
	@Override
	public Object applyBeanPostProcessorsAfterInitialization(Object bean, String name) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Object applyBeanPostProcessorsBeforeInitialization(Object bean, String name) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void applyBeanPropertyValues(Object bean, String name) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Object autowire(Class<?> clazz, int mode, boolean check) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void autowireBeanProperties(Object bean, int mode, boolean check) {
		// TODO Auto-generated method stub
		
	}

	@Override
	protected boolean containsBeanDefinition(String beanName) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	protected BeanDefinition getBeanDefinition(String beanName) throws BeansException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected Object createBean(String beanName, RootBeanDefinition mergedBeanDefinition, Object[] args)
			throws BeanCreationException {
		// TODO Auto-generated method stub
		return null;
	}

}
