package org.yuan.study.spring.beans.factory.xml;

import org.w3c.dom.Document;
import org.yuan.study.spring.beans.factory.support.BeanDefinitionReader;
import org.yuan.study.spring.core.io.Resource;

public interface XmlBeanDefinitionParser {

	/**
	 * Parse bean difinitions from the given DOM document, and register them with the given bean factory.
	 * @param reader
	 * @param doc
	 * @param resource
	 * @return
	 */
	int registerBeanDefinitions(BeanDefinitionReader reader, Document doc, Resource resource);
}
