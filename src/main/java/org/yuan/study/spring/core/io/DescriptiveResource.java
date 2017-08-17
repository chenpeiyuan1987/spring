package org.yuan.study.spring.core.io;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

public class DescriptiveResource extends AbstractResource {

	private final String description;

	/**
	 * Create a new DescriptiveResource.
	 * @param description
	 */
	public DescriptiveResource(String description) {
		this.description = (description != null ? description : "");
	}

	//-------------------------------------------------------------
	// Implementation of AbstractResource class
	//-------------------------------------------------------------
	
	@Override
	public String getDescription() {
		return description;
	}

	@Override
	public InputStream getInputStream() throws IOException {
		throw new FileNotFoundException(getDescription() + " cannot be opened because it does not point to a readable resource");
	}

	@Override
	public int hashCode() {
		return description.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		return (obj == this 
			|| (obj instanceof DescriptiveResource && ((DescriptiveResource) obj).description.equals(description)));
	}

}
