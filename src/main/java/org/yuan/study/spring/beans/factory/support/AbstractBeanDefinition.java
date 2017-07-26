package org.yuan.study.spring.beans.factory.support;

import org.yuan.study.spring.beans.MutablePropertyValues;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.beans.factory.config.ConstructorArgumentValues;

public abstract class AbstractBeanDefinition implements BeanDefinition {
	/**  */
	public static final int AUTOWIRE_AUTODETECT = 0;
	/**  */
	public static final int AUTOWIRE_BY_NAME = 1;
	/**  */
	public static final int AUTOWIRE_BY_TYPE = 2;
	/**  */
	public static final int AUTOWIRE_CONSTRUCTOR = 3;
	/**  */
	public static final int AUTOWIRE_NO = 4;
	/**  */
	public static final int DEPENDENCY_CHECK_ALL = 5;
	/**  */
	public static final int DEPENDENCY_CHECK_NONE = 6;
	/**  */
	public static final int DEPENDENCY_CHECK_OBJECTS = 7;
	/**  */
	public static final int DEPENDENCY_CHECK_SIMPLE = 8;
	
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
