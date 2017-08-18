package org.yuan.study.spring.core;

public interface Ordered {

	/**
	 * Return the order value of this object, 
	 * higher value meaning greater in terms of sorting.
	 * @return
	 */
	int getOrder();
}
