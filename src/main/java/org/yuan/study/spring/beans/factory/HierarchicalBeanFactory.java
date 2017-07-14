package org.yuan.study.spring.beans.factory;

public interface HierarchicalBeanFactory extends BeanFactory {
	
	/**
	 * Return whether the local bean factory contains a bean of the given name,
	 * ignoring beans defined in ancestor contexts.
	 * @param name
	 * @return
	 */
	boolean containsLocalBean(String name);
	
	/**
	 * Return the parent bean factory, or null if there is none.
	 * @return
	 */
	BeanFactory getParentBeanFactory();
}
