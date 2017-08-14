package org.yuan.study.spring.core.io;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

public abstract class AbstractResource implements Resource {

	//------------------------------------------------------
	// Implementation of Resource interface
	//------------------------------------------------------
	
	@Override
	public Resource createRelative(String relativePath) throws IOException {
		throw new FileNotFoundException("Cannot create a relative resource for " + getDescription());
	}

	@Override
	public boolean exists() {
		try {
			return getFile().exists();
		} 
		catch (Exception e) {
			try {
				InputStream is = getInputStream();
				is.close();
				return true;
			} 
			catch (Exception e2) {
				return false;
			}
		}
	}

	@Override
	public File getFile() throws IOException {
		throw new FileNotFoundException(getDescription() + " cannot be resolved to absolute file path");
	}

	@Override
	public String getFilename() {
		throw new IllegalStateException(getDescription() + " does not carry a filename");
	}

	@Override
	public URL getURL() throws IOException {
		throw new FileNotFoundException(getDescription() + " cannot be resolved to URL");
	}

	@Override
	public boolean isOpen() {
		return false;
	}
	
	//--------------------------------------------------
	// Implementation of Object class
	//--------------------------------------------------

	@Override
	public int hashCode() {
		return getDescription().hashCode();
	}

	@Override
	public String toString() {
		return getDescription();
	}

	@Override
	public boolean equals(Object obj) {
		return (obj == this || 
			(obj instanceof Resource && ((Resource) obj).getDescription().equals(getDescription())));
	}

}
