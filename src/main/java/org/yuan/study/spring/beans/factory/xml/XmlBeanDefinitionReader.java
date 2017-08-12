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

	/**
	 * 
	 * @param beanDefinitionRegistry
	 */
	public XmlBeanDefinitionReader(BeanDefinitionRegistry beanDefinitionRegistry) {
		super(beanDefinitionRegistry);
	}
	
	//----------------------------------------------------------------------
	// Implementation of methods
	//----------------------------------------------------------------------
	
	/**
	 * 
	 * @param encodedResource
	 * @return
	 */
	public int loadBeanDefinitions(EncodedResource encodedResource) {
		// TODO
		return 0;
	}
	
	/**
	 * 
	 * @param inputSource
	 * @return
	 */
	public int loadBeanDefinitions(InputSource inputSource) {
		// TODO
		return 0;
	}
	
	/**
	 * 
	 * @param inputSource
	 * @param resourceDescription
	 * @return
	 */
	public int loadBeanDefinitions(InputSource inputSource, String resourceDescription) {
		// TODO
		return 0;
	}
	
	/**
	 * 
	 * @param doc
	 * @param resource
	 * @return
	 */
	public int registerBeanDefinitions(Document doc, Resource resource) {
		// TODO
		return 0;
	}
	
	/**
	 * 
	 * @param entityResolver
	 */
	public void setEntityResolver(EntityResolver entityResolver) {
		// TODO
	}
	
	/**
	 * 
	 * @param errorHandler
	 */
	public void setErrorHandler(ErrorHandler errorHandler) {
		// TODO
	}
	
	/**
	 * 
	 * @param namespaceAware
	 */
	public void setNamespaceAware(boolean namespaceAware) {
		// TODO
	}
	
	/**
	 * 
	 * @param parserClass
	 */
	public void setParserClass(Class<?> parserClass) {
		// TODO
	}
	
	/**
	 * 
	 * @param validating
	 */
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
	
	/**
	 * 
	 * @return
	 */
	protected DocumentBuilderFactory createDocumentBuilderFactory() {
		// TODO
		return null;
	}
	
	/**
	 * 
	 * @param inputSource
	 * @param resource
	 * @return
	 */
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
