package org.yuan.study.spring.util;

import java.io.File;
import java.io.FileNotFoundException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;

public abstract class ResourceUtils {

	public static final String CLASSPATH_URL_PREFIX = "classpath:";
	
	public static final String FILE_URL_PREFIX = "file:";
	
	public static final String URL_PROTOCOL_FILE = "file";
	
	public static final String URL_PROTOCOL_JAR = "jar";
	public static final String URL_PROTOCOL_ZIP = "zip";
	public static final String URL_PROTOCOL_VFSZIP = "vfszip";
	public static final String URL_PROTOCOL_VFS = "vfs";
	public static final String URL_PROTOCOL_WSJAR = "wsjar";
	public static final String URL_PROTOCOL_CODE_SOURCE = "code-source";
	
	public static final String JAR_URL_SEPARATOR = "!/";
	
	/**
	 * Return whether the given resource location is a URL:
	 * either a special "classpath" pseudo URL or a standard URL.
	 * @param location
	 * @return
	 */
	public static boolean isUrl(String location) {
		if (location == null) {
			return false;
		}
		if (location.startsWith(CLASSPATH_URL_PREFIX)) {
			return true;
		}
		try {
			new URL(location);
			return true;
		}
		catch (MalformedURLException ex) {
			return false;
		}
	}
	
	/**
	 * Resolve the given resource URL to a java.io.File,
	 * i.e. to a file in the file system.
	 * @param url
	 * @param description
	 * @return
	 */
	public static File getFile(URL resourceUrl, String description) throws FileNotFoundException {
		Assert.notNull(resourceUrl, "Resource URL must not be null");
		if (!URL_PROTOCOL_FILE.equals(resourceUrl.getProtocol())) {
			throw new FileNotFoundException(String.format(
				"%s cannot be resolved to absolute file path because it does not reside in the file system: %s", description, resourceUrl));
		}
		return new File(URLDecoder.decode(resourceUrl.getFile()));
	}
}
