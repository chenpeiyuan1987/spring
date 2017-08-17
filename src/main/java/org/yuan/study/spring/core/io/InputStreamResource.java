package org.yuan.study.spring.core.io;

import java.io.IOException;
import java.io.InputStream;

import org.yuan.study.spring.util.Assert;

public class InputStreamResource extends AbstractResource {
	
	private final InputStream inputStream;
	
	private final String description;
	
	private boolean read = false;
	
	/**
	 * Create a new InputStreamResource.
	 * @param inputStream
	 */
	public InputStreamResource(InputStream inputStream) {
		this(inputStream, "resource loaded through InputStream");
	}
	
	/**
	 * Create a new InputStreamResource.
	 * @param inputStream
	 * @param description
	 */
	public InputStreamResource(InputStream inputStream, String description) {
		Assert.notNull(inputStream, "InputStream must not be null");
		this.inputStream = inputStream;
		this.description = (description != null ? description : "");
	}
	
	
	//----------------------------------------------------------------------------------
	// Implementation of AbstractResource class
	//----------------------------------------------------------------------------------

	@Override
	public String getDescription() {
		return description;
	}

	@Override
	public InputStream getInputStream() throws IOException {
		Assert.state(!read, "InputStream has already been read - do not use InputStreamResource if a stream needs to be read multiple times");
		read = true;
		return inputStream;
	}

	@Override
	public boolean exists() {
		return true;
	}

	@Override
	public boolean isOpen() {
		return true;
	}

	@Override
	public int hashCode() {
		return inputStream.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == this) {
			return true;
		}
		if (obj instanceof InputStreamResource) {
			InputStreamResource other = (InputStreamResource) obj;
			return other.inputStream.equals(inputStream);
		}
		return false;
	}
	
}
