package org.yuan.study.spring.core.io.support;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import org.yuan.study.spring.core.io.Resource;

public class EncodedResource {

	private final Resource resource;
	
	private final String encoding;

	/**
	 * Create a new EncodedResource for the given Resource,
	 * using the specified encoding.
	 * @param resource
	 */
	public EncodedResource(Resource resource) {
		this(resource, null);
	}
	
	/**
	 * Create a new EncodedResource for the given Resource,
	 * using the specified encoding.
	 * @param resource
	 * @param encoding
	 */
	public EncodedResource(Resource resource, String encoding) {
		this.resource = resource;
		this.encoding = encoding;
	}

	/**
	 * Return the Resource held.
	 * @return
	 */
	public Resource getResource() {
		return resource;
	}

	/**
	 * Return the encoding to use for reading from the resource,
	 * or null if none specified.
	 * @return
	 */
	public String getEncoding() {
		return encoding;
	}
	
	/**
	 * Open a java.io.Reader for the specified resource,
	 * using the specified encoding
	 * @return
	 */
	public Reader getReader() throws IOException {
		if (this.encoding != null) {
			return new InputStreamReader(this.resource.getInputStream(), this.encoding);
		} else {
			return new InputStreamReader(this.resource.getInputStream());
		}
	}
}
