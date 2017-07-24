package org.yuan.study.spring.beans.factory.support;

import java.util.HashSet;
import java.util.Set;

import org.yuan.study.spring.beans.BeanWrapper;
import org.yuan.study.spring.beans.BeanWrapperImpl;
import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.factory.BeanCreationException;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.config.AutowireCapableBeanFactory;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;

public abstract class AbstractAutowireCapableBeanFactory 
	extends AbstractBeanFactory implements AutowireCapableBeanFactory {
	
	private Instantia
	
	/***/
	private boolean allowCircularReferences = true;
	
	/***/
	private final Set<Class<?>> ignoredDependencyTypes = new HashSet<Class<?>>();
	
	/***/
	private final Set<Class<?>> ignoredDependencyInterfaces = new HashSet<Class<?>>();
	
	
	public AbstractAutowireCapableBeanFactory() {
		super();
	}

	public AbstractAutowireCapableBeanFactory(BeanFactory parentBeanFactory) {
		super(parentBeanFactory);
	}
	
	//-----------------------------------------------------------------
	// Implementation methods
	//-----------------------------------------------------------------
	
	/**
	 * 
	 * @return
	 */
	public boolean isAllowCircularReferences() {
		return allowCircularReferences;
	}

	/**
	 * 
	 * @param allowCircularReferences
	 */
	public void setAllowCircularReferences(boolean allowCircularReferences) {
		this.allowCircularReferences = allowCircularReferences;
	}

	/**
	 * 
	 * @return
	 */
	public Set<Class<?>> getIgnoredDependencyTypes() {
		return ignoredDependencyTypes;
	}

	/**
	 * 
	 * @return
	 */
	public Set<Class<?>> getIgnoredDependencyInterfaces() {
		return ignoredDependencyInterfaces;
	}
	
	/**
	 * 
	 * @param type
	 */
	public void ignoredDependencyType(Class<?> type) {
		this.ignoredDependencyTypes.add(type);
	}
	
	/**
	 * 
	 * @param ifc
	 */
	public void ignoredDependencyInterface(Class<?> ifc) {
		this.ignoredDependencyInterfaces.add(ifc);
	}
	
	/**
	 * 
	 * @throws BeansException
	 */
	protected void populateBean(String beanName, RootBeanDefinition mergedBeanDefinition, BeanWrapper bw) throws BeansException {
		
	}
	
	/**
	 * 
	 * @throws BeansException
	 */
	protected void autowireConstructor(String beanName, RootBeanDefinition mergedBeanDefinition) throws BeansException {
		
	}
	
	//-----------------------------------------------------------------
	// Implementation of AutowireCapableBeanFactory interface
	//-----------------------------------------------------------------
	
	@Override
	public Object applyBeanPostProcessorsAfterInitialization(Object existingBean, String beanName)
			throws BeansException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Object applyBeanPostProcessorsBeforeInitialization(Object existingBean, String beanName)
			throws BeansException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void applyBeanPropertyValues(Object existingBean, String beanName) throws BeansException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Object autowire(Class<?> beanClass, int autowireMode, boolean dependencyCheck) throws BeansException {
		RootBeanDefinition bd = new RootBeanDefinition(beanClass, autowireMode, dependencyCheck);
		bd.setSingleton(false);
		if (bd.getResolvedAutowireMode() == AUTOWIRE_CONSTRUCTOR) {
			return autowireConstructor(beanClass.getName(), bd).getWrappedInstance();
		}
		else {
			Object bean = null;
			populateBean(beanClass.getName(), bd, new BeanWrapperImpl(bean));
			return bean;
		}
		
		return null;
	}

	@Override
	public void autowireBeanProperties(Object existingBean, int autowireMode, boolean dependencyCheck)
			throws BeansException {
		// TODO Auto-generated method stub
		
	}
	
	
	//-----------------------------------------------------------------
	// Implementation of AbstractBeanFactory interface
	//-----------------------------------------------------------------

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
