package org.yuan.study.spring.beans.factory.annotation;

import org.yuan.study.spring.beans.factory.config.AutowireCapableBeanFactory;

public enum Autowire {
	
	NO(AutowireCapableBeanFactory.AUTOWIRE_NO),
	BY_NAME(AutowireCapableBeanFactory.AUTOWIRE_BY_NAME),
	BY_TYPE(AutowireCapableBeanFactory.AUTOWIRE_BY_TYPE);
	
	private final int value;
	
	Autowire(int value) {
		this.value = value;
	}
	
	public int value() {
		return value;
	}
	
	/**
	 * Return whether this represents an actual autowiring value.
	 * @return
	 */
	public boolean isAutowire() {
		if (this == BY_NAME) {
			return true;
		}
		if (this == BY_TYPE) {
			return true;
		}
		return false;
	}
}
