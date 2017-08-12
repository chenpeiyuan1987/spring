package org.yuan.study.spring.beans.factory.xml;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.springframework.core.io.support.EncodedResource;
import org.w3c.dom.Document;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.yuan.study.spring.beans.factory.support.AbstractBeanDefinitionReader;
import org.yuan.study.spring.beans.factory.support.BeanDefinitionRegistry;
import org.yuan.study.spring.core.io.Resource;

public class XmlBeanDefinitionReader extends AbstractBeanDefinitionReader {

	public XmlBeanDefinitionReader(BeanDefinitionRegistry beanDefinitionRegistry) {
		super(beanDefinitionRegistry);
	}
	
	//----------------------------------------------------------------------
	// Implementation of methods
	//----------------------------------------------------------------------
	
	public int loadBeanDefinitions(EncodedResource encodedResource) {
		// TODO
		return 0;
	}
	
	public int loadBeanDefinitions(InputSource inputSource) {
		// TODO
		return 0;
	}
	
	public int loadBeanDefinitions(InputSource inputSource, String resourceDescription) {
		// TODO
		return 0;
	}
	
	public int registerBeanDefinitions(Document doc, Resource resource) {
		// TODO
		return 0;
	}
	
	public void setEntityResolver(EntityResolver entityResolver) {
		// TODO
	}
	
	public void setErrorHandler(ErrorHandler errorHandler) {
		// TODO
	}
	
	public void setNamespaceAware(boolean namespaceAware) {
		// TODO
	}
	
	public void setParserClass(Class<?> parserClass) {
		// TODO
	}
	
	public void setValidating(boolean validating) {
		// TODO
	}
	
	/**
	 * 
	 * @param factory
	 * @return
	 */
	protected DocumentBuilder createDocumentBuilder(DocumentBuilderFactory factory) {
		// TODO
		return null;
	}
	
	protected DocumentBuilderFactory createDocumentBuilderFactory() {
		// TODO
		return null;
	}
	
	protected int doLoadBeanDefinitions(InputSource inputSource, Resource resource) {
		// TODO
		return 0;
	}
	

	//----------------------------------------------------------------------
	// Implementation of BeanDefinitionReader interface
	//----------------------------------------------------------------------

	@Override
	public int loadBeanDefinitions(Resource resource) {
		// TODO Auto-generated method stub
		return 0;
	}

}
