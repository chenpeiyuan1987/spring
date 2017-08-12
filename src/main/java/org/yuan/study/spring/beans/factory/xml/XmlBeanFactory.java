package org.yuan.study.spring.beans.factory.xml;

import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.support.DefaultListableBeanFactory;
import org.yuan.study.spring.core.io.Resource;

public class XmlBeanFactory extends DefaultListableBeanFactory {

	private final XmlBeanDefinitionReader reader = new XmlBeanDefinitionReader(this);
	
	/**
	 * Create a new XmlBeanFactory with the given resource,
	 * which must be parsable using DOM.
	 * @param resource
	 * @throws BeansException
	 */
	public XmlBeanFactory(Resource resource) throws BeansException {
		this(resource, null);
	}
	
	/**
	 * Create a new XmlBeanFactory with the given input stream,
	 * which must be parsable using DOM.
	 * @param resource
	 * @param parentBeanFactory
	 * @throws BeansException
	 */
	public XmlBeanFactory(Resource resource, BeanFactory parentBeanFactory) throws BeansException {
		super(parentBeanFactory);
		this.reader.loadBeanDefinitions(resource);
	}
}
