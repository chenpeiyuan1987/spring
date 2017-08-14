package org.yuan.study.spring.core.io;

import java.io.File;
import java.io.IOException;
import java.net.URL;

public interface Resource extends InputStreamSource {
	
	/**
	 * Create a resource relative to this resource.
	 * @param relativePath
	 * @return
	 */
	Resource createRelative(String relativePath) throws IOException;
	
	/**
	 * Return whether this resource actually exists in physical form.
	 * @return
	 */
	boolean exists();
	
	/**
	 * Return a description for this resource, 
	 * to be used for error output when working with the resource.
	 * @return
	 */
	String getDescription();
	
	/**
	 * Return a File handle for this resource.
	 * @return
	 */
	File getFile() throws IOException;
	
	/**
	 * Return a filename for this resource, 
	 * i.e. typically the last part of the path: for example, "myfile.txt".
	 * @return
	 */
	String getFilename();
	
	/**
	 * Return a URL handle for this resourcee.
	 * @return
	 */
	URL getURL() throws IOException;
	
	/**
	 * Return whether this resource represents a handle with an open stream.
	 * @return
	 */
	boolean isOpen();
}
