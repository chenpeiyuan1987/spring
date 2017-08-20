package org.yuan.study.spring.core.io;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.ObjectUtils;
import org.yuan.study.spring.util.ResourceUtils;
import org.yuan.study.spring.util.StringUtils;

public class ClassPathResource extends AbstractResource {
	
	private final String path;
	
	private ClassLoader classLoader;
	
	private Class<?> clazz;
	
	/**
	 * Create a new ClassPathResource for ClassLoader usage.
	 * @param path
	 */
	public ClassPathResource(String path) {
		this(path, (ClassLoader) null);
	}

	/**
	 * Create a new ClassPathResource for ClassLoader usage.
	 * @param path
	 * @param classLoader
	 */
	public ClassPathResource(String path, ClassLoader classLoader) {
		Assert.notNull(path, "Path must not be null");
		if (path.startsWith("/")) {
			path = path.substring(1);
		} 
		this.path = StringUtils.cleanPath(path);
		this.classLoader = (classLoader != null ? classLoader : ClassUtils.getDefaultClassLoader());
	}
	
	/**
	 * Create a new ClassPathResource for ClassLoader usage.
	 * @param path
	 * @param clazz
	 */
	public ClassPathResource(String path, Class<?> clazz) {
		Assert.notNull(path, "Path must not be null");
		this.path = StringUtils.cleanPath(path);
		this.clazz = clazz;
	}
	
	/**
	 * Create a new ClassPathResource for ClassLoader usage.
	 * @param path
	 * @param classLoader
	 * @param clazz
	 */
	protected ClassPathResource(String path, ClassLoader classLoader, Class<?> clazz) {
		this.path = path;
		this.clazz = clazz;
		this.classLoader = classLoader;
	}

	/**
	 * Return the path for this resource.
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
		return String.format("class path resource [%s]", path);
	}

	@Override
	public InputStream getInputStream() throws IOException {
		InputStream is = null;
		if (clazz != null) {
			is = clazz.getResourceAsStream(path);
		} else {
			is = classLoader.getResourceAsStream(path);
		}
		if (is == null) {
			throw new FileNotFoundException(getDescription() + " cannot be opened because it does not exist");
		}
		return is;
	}

	@Override
	public Resource createRelative(String relativePath) throws IOException {
		String pathToUse = StringUtils.applyRelativePath(this.path, relativePath);
		return new ClassPathResource(pathToUse, classLoader, clazz);
	}

	@Override
	public File getFile() throws IOException {
		return ResourceUtils.getFile(getURL(), getDescription());
	}

	@Override
	public String getFilename() {
		return StringUtils.getFilename(path);
	}

	@Override
	public URL getURL() throws IOException {
		URL url = null;
		if (clazz != null) {
			url = clazz.getResource(path);
		}
		else {
			url = classLoader.getResource(path);
		}
		if (url == null) {
			throw new FileNotFoundException(
				getDescription() + " cannot be resolved to URL because it does not exist");
		}
		return url;
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
		if (obj instanceof ClassPathResource) {
			ClassPathResource other = (ClassPathResource) obj;
			return (path.equals(other.path) 
				&& ObjectUtils.nullSafeEquals(classLoader, other.classLoader) 
				&& ObjectUtils.nullSafeEquals(clazz, other.clazz));
		}
		return false;
	}

}
