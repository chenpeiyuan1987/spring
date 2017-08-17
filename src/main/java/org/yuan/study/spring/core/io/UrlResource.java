package org.yuan.study.spring.core.io;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;

import org.springframework.util.Assert;
import org.springframework.util.ResourceUtils;
import org.springframework.util.StringUtils;

public class UrlResource extends AbstractResource {
	
	private final URL url;
	
	private final URL cleanedUrl;
	
	
	/**
	 * Create a new UrlResource.
	 * @param url
	 */
	public UrlResource(URL url) {
		Assert.notNull(url, "URL must not be null");
		this.url = url;
		this.cleanedUrl = getCleanedUrl(url, url.toString());
	}
	
	/**
	 * Create a new UrlResource.
	 * @param url
	 */
	public UrlResource(String path) throws MalformedURLException {
		Assert.notNull(path, "Path must not be null");
		this.url = new URL(path);
		this.cleanedUrl = getCleanedUrl(url, path);
	}
	
	/**
	 * Determine a  cleaned URL for the given original URL.
	 * @param originalUrl
	 * @param originalPath
	 * @return
	 */
	private URL getCleanedUrl(URL originalUrl, String originalPath) {
		try {
			return new URL(StringUtils.cleanPath(originalPath));
		}
		catch (MalformedURLException ex) {
			return originalUrl;
		}
	}
	
	
	//----------------------------------------------------------------------------------
	// Implementation of AbstractResource class
	//----------------------------------------------------------------------------------
	
	@Override
	public String getFilename() {
		return new File(url.getFile()).getName();
	}
	
	@Override
	public URL getURL() throws IOException {
		return url;
	}
	
	@Override
	public File getFile() throws IOException {
		return ResourceUtils.getFile(url, getDescription());
	}
	
	@Override
	public Resource createRelative(String relativePath) throws MalformedURLException {
		if (relativePath.startsWith("/")) {
			relativePath = relativePath.substring(1);
		}
		return new UrlResource(new URL(url, relativePath));
	}
	
	@Override
	public String getDescription() {
		return String.format("URL [%s]", url);
	}

	@Override
	public InputStream getInputStream() throws IOException {
		URLConnection con = url.openConnection();
		con.setUseCaches(false);
		return con.getInputStream();
	}

	@Override
	public int hashCode() {
		return cleanedUrl.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		return (obj == this || (obj instanceof UrlResource && this.cleanedUrl.equals(((UrlResource) obj).cleanedUrl)));
	}
	
}
