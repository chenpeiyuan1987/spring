package org.yuan.study.spring.beans.factory.support;


public class RootBeanDefinition extends AbstractBeanDefinition {

	public RootBeanDefinition(RootBeanDefinition rootBeanDefinition) {
		super();
		// TODO
	}
	
	public RootBeanDefinition(Class<?> beanClass, int autowireMode, boolean dependencyCheck) {
		super();
		// TODO
	}
	
	public boolean hasConstructorArgumentValues() {
		return false;
	}

}
