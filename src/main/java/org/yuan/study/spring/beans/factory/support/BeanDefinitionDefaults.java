package org.yuan.study.spring.beans.factory.support;

public class BeanDefinitionDefaults {

	private boolean lazyInit;
	
	private int dependencyCheck = AbstractBeanDefinition.DEPENDENCY_CHECK_NONE;
	
	private int autowireMode = AbstractBeanDefinition.AUTOWIRE_NO;
	
	private String initMethodName;
	
	private String destroyMethodName;

	public boolean isLazyInit() {
		return lazyInit;
	}

	public void setLazyInit(boolean lazyInit) {
		this.lazyInit = lazyInit;
	}

	public int getDependencyCheck() {
		return dependencyCheck;
	}

	public void setDependencyCheck(int dependencyCheck) {
		this.dependencyCheck = dependencyCheck;
	}

	public int getAutowireMode() {
		return autowireMode;
	}

	public void setAutowireMode(int autowireMode) {
		this.autowireMode = autowireMode;
	}

	public String getInitMethodName() {
		return initMethodName;
	}

	public void setInitMethodName(String initMethodName) {
		this.initMethodName = initMethodName;
	}

	public String getDestroyMethodName() {
		return destroyMethodName;
	}

	public void setDestroyMethodName(String destroyMethodName) {
		this.destroyMethodName = destroyMethodName;
	}
	
}
