package org.yuan.study.spring.core.io;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ResourceUtils;
import org.yuan.study.spring.util.StringUtils;


public class FileSystemResource extends AbstractResource {
	
	private final File file;
	
	private final String path;
	
	/**
	 * Create a new FileSystemResource.
	 * @param file
	 */
	public FileSystemResource(File file) {
		Assert.notNull(file, "File must not be null");
		this.file = file;
		this.path = StringUtils.cleanPath(file.getPath());
	}
	
	/**
	 * Create a new FileSystemResource.
	 * @param path
	 */
	public FileSystemResource(String path) {
		Assert.notNull(path, "Path must no be null");
		this.file = new File(path);
		this.path = StringUtils.cleanPath(path);
	}
	
	/**
	 * Return the file path for this resource.
	 * @return
	 */
	public String getPath() {
		return path;
	}
	
	
	//----------------------------------------------------------------------------------
	// Implementation of AbstractResource class
	//----------------------------------------------------------------------------------

	@Override
	public String getDescription() {
		return String.format("file [%s]", file.getAbsolutePath());
	}

	@Override
	public InputStream getInputStream() throws IOException {
		return new FileInputStream(file);
	}

	@Override
	public Resource createRelative(String relativePath) throws IOException {
		String pathToUse = StringUtils.applyRelativePath(path, relativePath);
		return new FileSystemResource(pathToUse);
	}

	@Override
	public String getFilename() {
		return file.getName();
	}

	@Override
	public int hashCode() {
		return path.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == this) {
			return true;
		}
		if (obj instanceof FileSystemResource) {
			FileSystemResource other = (FileSystemResource) obj;
			return path.equals(other.path);
		}
		return false;
	}

	@Override
	public boolean exists() {
		return file.exists();
	}

	@Override
	public File getFile() throws IOException {
		return file;
	}

	@Override
	public URL getURL() throws IOException {
		return new URL(ResourceUtils.FILE_URL_PREFIX + this.file.getAbsolutePath());
	}

}
