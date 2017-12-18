package org.yuan.study.spring.core.io;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

import org.yuan.study.spring.core.NestedIOException;
import org.yuan.study.spring.util.ResourceUtils;

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
	
	@Override
	public boolean isReadable() {
		return true;
	}

	@Override
	public URI getURI() throws IOException {
		URL url = getURL();
		try {
			return ResourceUtils.toURI(url);
		} 
		catch (URISyntaxException ex) {
			throw new NestedIOException(String.format("Invalid URI [%s]", url), ex);
		}
	}

	@Override
	public long contentLength() throws IOException {
		return getFile().length();
	}

	@Override
	public long lastModified() throws IOException {
		long lastModified = getFileForLastModifiedCheck().lastModified();
		if (lastModified == 0L) {
			throw new FileNotFoundException(getDescription() 
				+ " cannot be resolved in the file system for resolving its last-modified timestamp");
		}
		return lastModified;
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

	/**
	 * Determine the File to use for timestamp checking.
	 * @return
	 * @throws IOException
	 */
	protected File getFileForLastModifiedCheck() throws IOException {
		return getFile();
	}
}
