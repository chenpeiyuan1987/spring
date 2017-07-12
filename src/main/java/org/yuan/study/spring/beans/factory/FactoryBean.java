package org.yuan.study.spring.beans.factory;

public interface FactoryBean {

	/**
	 * Return an instance of the object managed by this factory.
	 * @return
	 */
	Object getObject();
	
	/**
	 * Return the type of object that this FactoryBean creates, or null if not known in advance.
	 * @return
	 */
	Class<?> getObjectType();
	
	/**
	 * Is the bean managed by this factory a singleton or a prototype?
	 * @return
	 */
	boolean isSingleton();
}
