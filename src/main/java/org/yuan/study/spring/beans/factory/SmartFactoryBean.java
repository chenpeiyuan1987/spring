package org.yuan.study.spring.beans.factory;

public interface SmartFactoryBean<T> extends FactoryBean<T> {

	/**
	 * Is the object managed by this factory a prototype? That is,
	 * will always return an independent instance?
	 * @return
	 */
	boolean isPrototype();
	
	/**
	 * Does this FactoryBean expect eager initialization, that is,
	 * eagerly initialize itself as well as expect eager initialization
	 * of its singleton object ?
	 * @return
	 */
	boolean isEagerInit();
}
