package org.yuan.study.spring.beans.factory;

import org.yuan.study.spring.beans.BeansException;

public interface ObjectFactory<T> {

	/**
	 * Return an instance (possibly shared or independent)
	 * of the object managed by this factory.
	 * @return
	 * @throws BeansException
	 */
	T getObject() throws BeansException;
}
