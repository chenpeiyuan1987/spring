package org.yuan.study.spring.util.xml;

import org.apache.commons.logging.Log;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

public class SimpleSaxErrorHandler implements ErrorHandler {
	
	private final Log logger; 
	
	/**
	 * Create a new SimpleSaxErrorHandler for the given Commons Logging logger instance.
	 * @param logger
	 */
	public SimpleSaxErrorHandler(Log logger) {
		this.logger = logger;
	}

	@Override
	public void warning(SAXParseException ex) throws SAXException {
		logger.warn(String.format("Ignored XML validation warning: %s", ex.getMessage()), ex);
	}

	@Override
	public void error(SAXParseException exception) throws SAXException {
		throw exception;
	}

	@Override
	public void fatalError(SAXParseException exception) throws SAXException {
		throw exception;
	}

}
