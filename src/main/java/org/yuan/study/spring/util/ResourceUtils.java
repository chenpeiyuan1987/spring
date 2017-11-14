package org.yuan.study.spring.util;

import java.io.File;
import java.io.FileNotFoundException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

public abstract class ResourceUtils {

	/** Pseudo URL prefix for loading from the class path: "classpath:" */
	public static final String CLASSPATH_URL_PREFIX = "classpath:";
	
	/** URL prefix for loading from the file system: "file:" */
	public static final String FILE_URL_PREFIX = "file:";
	
	/** URL protocol for a file in the file system: "file" */
	public static final String URL_PROTOCOL_FILE = "file";
	
	/** URL protocol for an entry from a jar file: "jar" */
	public static final String URL_PROTOCOL_JAR = "jar";
	
	/** URL protocol for an entry from a zip file: "zip" */
	public static final String URL_PROTOCOL_ZIP = "zip";
	
	/** URL protocol for an entry from a JBoss jar file: "vfszip" */
	public static final String URL_PROTOCOL_VFSZIP = "vfszip";
	
	/** URL protocol for a JBoss VFS resource: "vfs" */
	public static final String URL_PROTOCOL_VFS = "vfs";
	
	/** URL protocol for an entry from a WebSphere jar file: "wsjar" */
	public static final String URL_PROTOCOL_WSJAR = "wsjar";
	
	/** URL protocol for an entry from an OC4J jar file: "code-source" */
	public static final String URL_PROTOCOL_CODE_SOURCE = "code-source";
	
	/** Separator between JAR URL and file path within the JAR */
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
	 * Resolve the given resource location to a java.net.URL.
	 * Does not check whether the URL actually exists; simple 
	 * returns the URL that the given location would correspond to.
	 * @param location
	 * @return
	 * @throws FileNotFoundException
	 */
	public static URL getURL(String location) throws FileNotFoundException {
		Assert.notNull(location, "Location must not be null");
		
		if (location.startsWith(CLASSPATH_URL_PREFIX)) {
			String path = location.substring(CLASSPATH_URL_PREFIX.length());
			URL url = ClassUtils.getDefaultClassLoader().getResource(path);
			if (url == null) {
				String desc = String.format("class path resource [%s]", path);
				throw new FileNotFoundException(desc + " cannot be resolved to URL because it does not exist");
			}
			return url;
		}
		try {
			return new URL(location);
		} 
		catch (MalformedURLException e) {
			try {
				return new File(location).toURI().toURL();
			} 
			catch (MalformedURLException e2) {
				throw new FileNotFoundException(String.format(
					"Resource location [%s] is neither a URL not a well-formed file path", location));
			}
		}
	}
	
	/**
	 * Resolve the given resource location to a java.io.File,
	 * i.e. to a file in the file system.
	 * @param location
	 * @return
	 * @throws FileNotFoundException
	 */
	public static File getFile(String location) throws FileNotFoundException {
		Assert.notNull(location, "Location must not be null");
		
		if (location.startsWith(CLASSPATH_URL_PREFIX)) {
			String path = location.substring(CLASSPATH_URL_PREFIX.length());
			String desc = String.format("class path resource [%s]", path);
			URL url = ClassUtils.getDefaultClassLoader().getResource(path);
			if (url == null) {
				throw new FileNotFoundException(desc + " cannot be resolved to absolute file path because it does not reside in the file system");
			}
			return getFile(url, desc);
		}
		try {
			return getFile(new URL(location));
		}
		catch (MalformedURLException ex) {
			return new File(location);
		}
	}
	
	/**
	 * Determine whether the given URL points to a resource in the file system,
	 * that is, has protocol "file" or "vfs".
	 * @param url
	 * @return
	 * @throws FileNotFoundException
	 */
	public static File getFile(URL url) throws FileNotFoundException {
		return getFile(url, "URL");
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
		try {
			return new File(toURI(resourceUrl).getSchemeSpecificPart());
		}
		catch (URISyntaxException ex) {
			return new File(resourceUrl.getFile());
		}
	}
	
	/**
	 * Resolve the given resource URI to a java.io.File,
	 * i.e. to a file in the file system.
	 * @param resourceUri
	 * @return
	 * @throws FileNotFoundException
	 */
	public static File getFile(URI resourceUri) throws FileNotFoundException {
		return getFile(resourceUri, "URI");
	}
	
	/**
	 * Resolve the given resource URI to a java.io.File,
	 * i.e. to a file in the file system.
	 * @param resourceUri
	 * @param description
	 * @return
	 * @throws FileNotFoundException
	 */
	public static File getFile(URI resourceUri, String description) throws FileNotFoundException {
		Assert.notNull(resourceUri, "Resource URI must not be null");
		
		if (!URL_PROTOCOL_FILE.equals(resourceUri.getScheme())) {
			throw new FileNotFoundException(description + " cannot be resolved to absolute file path because it does not reside in the file system: " + resourceUri);
		}
		return new File(resourceUri.getSchemeSpecificPart());
	}
	
	/**
	 * Determine whether the given URL points to a resource in the file system,
	 * that is, has protocol "file" or "vfs".
	 * @param url
	 * @return
	 */
	public static boolean  isFileURL(URL url) {
		String protocol = url.getProtocol();
		return (URL_PROTOCOL_FILE.equals(protocol) || protocol.startsWith(URL_PROTOCOL_VFS));
	}
	
	/**
	 * Determine whether the given URL points to a resource in a jar file,
	 * that is, has protocol "jar", "zip", "wsjar" or "code-source".
	 * @param url
	 * @return
	 */
	public static boolean isJarURL(URL url) {
		String protocol = url.getProtocol();
		return (URL_PROTOCOL_JAR.equals(protocol) || URL_PROTOCOL_ZIP.equals(protocol) || URL_PROTOCOL_WSJAR.equals(protocol) || 
			(URL_PROTOCOL_CODE_SOURCE.equals(protocol) && url.getPath().contains(JAR_URL_SEPARATOR)));
	}
	
	/**
	 * Extract the URL for the actual jar file from the given URL.
	 * @param jarUrl
	 * @return
	 * @throws MalformedURLException
	 */
	public static URL extractJarFileURL(URL jarUrl) throws MalformedURLException {
		String urlFile = jarUrl.getFile();
		int index = urlFile.indexOf(JAR_URL_SEPARATOR);
		if (index != -1) {
			String jarFile = urlFile.substring(0, index);
			try {
				return new URL(jarFile);
			}
			catch (MalformedURLException ex) {
				if (!jarFile.startsWith("/")) {
					jarFile = "/" + jarFile;
				}
				return new URL(FILE_URL_PREFIX + jarFile);
			}
		} 
		else {
			return jarUrl;
		}
	}
	
	/**
	 * Create a URI instance for the given URL,
	 * replacing spaces with "%20" quotes first.
	 * @param url
	 * @return
	 * @throws URISyntaxException
	 */
	public static URI toURI(URL url) throws URISyntaxException {
		return toURI(url.toString());
	}
	
	/**
	 * Create a URI instance for the given location String,
	 * replacing spaces with "%20" quotes first.
	 * @param location
	 * @return
	 * @throws URISyntaxException
	 */
	public static URI toURI(String location) throws URISyntaxException {
		return new URI(StringUtils.replace(location, " ", "%20"));
	}
}
