package org.yuan.study.spring.beans.factory.xml;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Document;
import org.yuan.study.spring.beans.factory.support.BeanDefinitionReader;
import org.yuan.study.spring.core.io.Resource;

public class DefaultXmlBeanDefinitionParser implements XmlBeanDefinitionParser {
	
	protected final Log logger = LogFactory.getLog(getClass());
	
	private BeanDefinitionReader beanDefinitionReader;
	
	private Resource resource;
	
	private String defaultLazyInit;
	
	private String defaultAutowire;
	
	private String defaultDependencyCheck;
	
	private String defaultInitMethod;
	
	private String defaultDestroyMethod;
	
	

	//--------------------------------------------------------------------------
	// Implementation of methods
	//--------------------------------------------------------------------------
	
	protected final BeanDefinitionReader getBeanDefinitionReader() {
		return beanDefinitionReader;
	}
	
	protected final Resource getResource() {
		return resource;
	}
	
	//--------------------------------------------------------------------------
	// Implementation of XmlBeanDefinitionParser interface
	//--------------------------------------------------------------------------
	
	@Override
	public int registerBeanDefinitions(BeanDefinitionReader reader, Document doc, Resource resource) {
		// TODO Auto-generated method stub
		return 0;
	}

}
