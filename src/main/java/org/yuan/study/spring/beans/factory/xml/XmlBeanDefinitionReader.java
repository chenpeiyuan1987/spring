package org.yuan.study.spring.beans.factory.xml;

import java.io.IOException;
import java.io.InputStream;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.springframework.core.io.DescriptiveResource;
import org.springframework.util.xml.SimpleSaxErrorHandler;
import org.w3c.dom.Document;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.yuan.study.spring.beans.BeanUtils;
import org.yuan.study.spring.beans.factory.BeanDefinitionStoreException;
import org.yuan.study.spring.beans.factory.support.AbstractBeanDefinitionReader;
import org.yuan.study.spring.beans.factory.support.BeanDefinitionRegistry;
import org.yuan.study.spring.core.io.Resource;
import org.yuan.study.spring.core.io.support.EncodedResource;

public class XmlBeanDefinitionReader extends AbstractBeanDefinitionReader {
	
	private boolean validating = true;
	
	private boolean namespaceAware = false;
	
	private ErrorHandler errorHandler = new SimpleSaxErrorHandler(logger);
	
	private EntityResolver entityResolver = null;
	
	private Class<?> parserClass = DefaultXmlBeanDefinitionParser.class;

	/**
	 * Create new XmlBeanDefinitionReader for the given bean factory.
	 * @param beanDefinitionRegistry
	 */
	public XmlBeanDefinitionReader(BeanDefinitionRegistry beanDefinitionRegistry) {
		super(beanDefinitionRegistry);
	}
	
	//----------------------------------------------------------------------
	// Implementation of methods
	//----------------------------------------------------------------------
	
	/**
	 * Load bean definitions from the specified XML file.
	 * @param encodedResource
	 * @return
	 */
	public int loadBeanDefinitions(EncodedResource encodedResource) {
		if (encodedResource == null) {
			throw new BeanDefinitionStoreException("Resource cannot be null: expected an XML file");
		}
		if (logger.isInfoEnabled()) {
			logger.info(String.format("Loading XML bean definition from %s", encodedResource.getResource()));
		}
		try {
			InputStream inputStream = encodedResource.getResource().getInputStream();
			try {
				InputSource inputSource = new InputSource(inputStream);
				if (encodedResource.getEncoding() != null) {
					inputSource.setEncoding(encodedResource.getEncoding());
				}
				return doLoadBeanDefinitions(inputSource, encodedResource.getResource());
			}
			finally {
				inputStream.close();
			}
		}
		catch (IOException ex) {
			throw new BeanDefinitionStoreException(String.format("IOException parsing XML document from %s", encodedResource.getResource()), ex);
		}
	}
	
	/**
	 * Load bean definitions from the specified XML file.
	 * @param inputSource
	 * @return
	 */
	public int loadBeanDefinitions(InputSource inputSource) {
		return loadBeanDefinitions(inputSource, "resource loaded through SAX InputSource");
	}
	
	/**
	 * Load bean definitions from the specified XML file.
	 * @param inputSource
	 * @param resourceDescription
	 * @return
	 */
	public int loadBeanDefinitions(InputSource inputSource, String resourceDescription) {
		return doLoadBeanDefinitions(inputSource, new DescriptiveResource(resourceDescription));
	}
	
	/**
	 * Register the bean definitions contained in the given DOM document.
	 * @param doc
	 * @param resource
	 * @return
	 */
	public int registerBeanDefinitions(Document doc, Resource resource) {
		XmlBeanDefinitionParser parser = (XmlBeanDefinitionParser) BeanUtils.instantiateClass(this.parserClass);
		return parser.registerBeanDefinitions(this, doc, resource);
	}
	
	/**
	 * Set a SAX entity resolver to be used for parsing. By default,
	 * BeansDtdResolver will be used. Can be overridden for custom entity 
	 * resolution, for example relative to some specific base path.
	 * @param entityResolver
	 */
	public void setEntityResolver(EntityResolver entityResolver) {
		this.entityResolver = entityResolver;
	}
	
	/**
	 * Set an implementation of the org.xml.sax.ErrorHandler interface 
	 * for custom handling of XML parsing errors and warnings.
	 * @param errorHandler
	 */
	public void setErrorHandler(ErrorHandler errorHandler) {
		this.errorHandler = errorHandler;
	}
	
	/**
	 * Set whether or not the XML parser should be XML namespace aware.
	 * Default is "false".
	 * @param namespaceAware
	 */
	public void setNamespaceAware(boolean namespaceAware) {
		this.namespaceAware = namespaceAware;
	}
	
	/**
	 * Set the XmlBeanDefinitionParser implementation to use,
	 * responsible for the actual parsing of XML bean definitions.
	 * Default is DefaultXmlBeanDefinitionParser.
	 * @param parserClass
	 */
	public void setParserClass(Class<?> parserClass) {
		if (this.parserClass == null || !XmlBeanDefinitionParser.class.isAssignableFrom(parserClass)) {
			throw new IllegalArgumentException("parserClass must be an XmlBeanDefinitionParser");
		}
		this.parserClass = parserClass;
	}
	
	/**
	 * Set if the XML parser should validate the document and thus enforce a DTD.
	 * Default is true.
	 * @param validating
	 */
	public void setValidating(boolean validating) {
		this.validating = validating;
	}
	
	/**
	 * Create a JAXP DocumentBuilderFactory that this bean definition reader will use for parsing XML documents.
	 * Can be overridden in subclasses, adding further initialization of the factory.
	 * @param factory
	 * @return
	 */
	protected DocumentBuilder createDocumentBuilder(DocumentBuilderFactory factory) throws ParserConfigurationException {
		DocumentBuilder docBuilder = factory.newDocumentBuilder();
		if (this.errorHandler != null) {
			docBuilder.setErrorHandler(this.errorHandler);
		}
		if (this.entityResolver != null) {
			docBuilder.setEntityResolver(this.entityResolver);
		}
		return docBuilder;
	}
	
	/**
	 * Create a JAXP DocumentBuilderFactory that this bean definition reader will use for parsing XML documents.
	 * Can be overridden in subclasses, adding further initialization of the factory.
	 * @return
	 */
	protected DocumentBuilderFactory createDocumentBuilderFactory() throws ParserConfigurationException {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setValidating(this.validating);
		factory.setNamespaceAware(this.namespaceAware);
		return factory;
	}
	
	/**
	 * Actually load bean definitions from the specified XML file.
	 * @param inputSource
	 * @param resource
	 * @return
	 */
	protected int doLoadBeanDefinitions(InputSource inputSource, Resource resource) {
		try {
			DocumentBuilderFactory factory = createDocumentBuilderFactory();
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("Using JAXP implementation [%s]", factory));
			}
			DocumentBuilder builder = createDocumentBuilder(factory);
			Document doc = builder.parse(inputSource);
			return registerBeanDefinitions(doc, resource);
		}
		catch (ParserConfigurationException ex) {
			throw new BeanDefinitionStoreException(String.format("Parser configuration exception parsing XML from %s", resource), ex);
		}
		catch (SAXParseException ex) {
			throw new BeanDefinitionStoreException(String.format("Line %s in XML document from %s is invalid", ex.getLineNumber(), resource), ex);
		}
		catch (SAXException ex) {
			throw new BeanDefinitionStoreException(String.format("XML document from %s is invalid", resource), ex);
		}
		catch (IOException ex) {
			throw new BeanDefinitionStoreException(String.format("IOException parsing XML document from %s", resource), ex);
		}
	}
	

	//----------------------------------------------------------------------
	// Implementation of BeanDefinitionReader interface
	//----------------------------------------------------------------------

	@Override
	public int loadBeanDefinitions(Resource resource) {
		return loadBeanDefinitions(new EncodedResource(resource));
	}

}
