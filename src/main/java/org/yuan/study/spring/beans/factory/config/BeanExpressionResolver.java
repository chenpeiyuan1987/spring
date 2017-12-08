package org.yuan.study.spring.beans.factory.config;

import org.yuan.study.spring.beans.BeansException;

public interface BeanExpressionResolver {

	/**
	 * Evaluate the given value as an expression, if applicable;
	 * return the value as-is otherwise.
	 * @param value
	 * @param evalContext
	 * @return
	 * @throws BeansException
	 */
	Object evaluate(String value, BeanExpressionContext evalContext) throws BeansException;
}
