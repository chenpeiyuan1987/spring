package org.yuan.study.spring.beans.factory.support;

import org.yuan.study.spring.beans.MutablePropertyValues;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.beans.factory.config.ConstructorArgumentValues;

public abstract class AbstractBeanDefinition implements BeanDefinition {
	
	//----------------------------------------------------------------
	// Implementation methods
	//----------------------------------------------------------------
	
	/**
	 * 
	 * @param other
	 */
	public void overrideFrom(AbstractBeanDefinition other) {
		// TODO
	}
	
	/**
	 * 
	 * @throws BeanDefinitionValidationException
	 */
	public void validate() throws BeanDefinitionValidationException {
		// TODO
	}
	
	/**
	 * 
	 * @return
	 */
	public boolean hasBeanClass() {
		// TODO
		return false;
	}
	
	/**
	 * 
	 * @return
	 * @throws IllegalStateException
	 */
	public Class<?> getBeanClass() throws IllegalStateException {
		// TODO
		return null;
	}
	
	/**
	 * 
	 * @return
	 */
	public String getFactoryMethodName() {
		// TODO
		return null;
	}
	
	/**
	 * 
	 * @return
	 */
	public String getDestroyMethodName() {
		// TODO
		return null;
	}
	
	/**
	 * 
	 * @return
	 */
	public boolean isEnforceDestroyMethod() {
		// TODO
		return false;
	}
	
	/**
	 * 
	 * @return
	 */
	public String[] getDependsOn() {
		return null;
	}
	
	/**
	 * 
	 * @param singleton
	 */
	public void setSingleton(boolean singleton) {
		
	}
	
	/**
	 * 
	 * @return
	 */
	public int getResolvedAutowireMode() {
		return 0;
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
