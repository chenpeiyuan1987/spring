package org.yuan.study.spring.core.io.support;

import java.io.File;
import java.io.IOException;
import java.net.JarURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.util.ResourceUtils;
import org.yuan.study.spring.core.CollectionFactory;
import org.yuan.study.spring.core.io.DefaultResourceLoader;
import org.yuan.study.spring.core.io.FileSystemResource;
import org.yuan.study.spring.core.io.Resource;
import org.yuan.study.spring.core.io.ResourceLoader;
import org.yuan.study.spring.core.io.UrlResource;
import org.yuan.study.spring.util.AntPathMatcher;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.PathMatcher;
import org.yuan.study.spring.util.StringUtils;

public class PathMatchingResourcePatternResolver implements ResourcePatternResolver {
	
	/** URL protocol for an entry from a jar file: "jar" */
	private static final String URL_PROTOCOL_JAR = "jar";
	
	/** URL protocol for an entry from a zip file: "zip" */
	private static final String URL_PROTOCOL_ZIP = "zip";
	
	/** URL protocol for an entry from a WebSphere jar file: "wsjar" */
	private static final String URL_PROTOCOL_WSJAR = "wsjar";
	
	/** Separator between JAR URL and file path within the JAR */
	private static final String JAR_URL_SEPARATOR = "!/";

	protected final Log logger = LogFactory.getLog(getClass());
	
	private final ResourceLoader resourceLoader;
	
	private ClassLoader classLoader;
	
	private PathMatcher pathMatcher = new AntPathMatcher();
	
	/**
	 * Create a new PathMatchingResourcePatternResolver
	 */
	public PathMatchingResourcePatternResolver() {
		this(new DefaultResourceLoader(), null);
	}

	/**
	 * Create a new PathMatchingResourcePatternResolver
	 * @param classLoader
	 */
	public PathMatchingResourcePatternResolver(ClassLoader classLoader) {
		this(new DefaultResourceLoader(classLoader), classLoader);
	}

	/**
	 * Create a new PathMatchingResourcePatternResolver
	 * @param resourceLoader
	 */
	public PathMatchingResourcePatternResolver(ResourceLoader resourceLoader) {
		this(resourceLoader, null);
	}

	/**
	 * Create a new PathMatchingResourcePatternResolver
	 * @param resourceLoader
	 * @param classLoader
	 */
	public PathMatchingResourcePatternResolver(ResourceLoader resourceLoader, ClassLoader classLoader) {
		Assert.notNull(resourceLoader, "ResourceLoader must not be null");
		this.resourceLoader = resourceLoader;
		this.classLoader = (classLoader != null ? classLoader : ClassUtils.getDefaultClassLoader());
	}
	
	/**
	 * Return the ResourceLoader that this pattern resolver works with.
	 * @return
	 */
	public ResourceLoader getResourceLoader() {
		return resourceLoader;
	}
	
	/**
	 * Return the PathMatcher that this resource pattern resolver uses.
	 * @return
	 */
	public PathMatcher getPathMatcher() {
		return pathMatcher;
	}
	
	/**
	 * Set the PathMatcher Implementation to use for this resource pattern resolver.
	 * Default is AntPathMatcher.
	 * @param pathMatcher
	 */
	public void setPathMatcher(PathMatcher pathMatcher) {
		Assert.notNull(pathMatcher, "PathMatcher must not be null");
		this.pathMatcher = pathMatcher;
	}

	/**
	 * Return the ClassLoader that this pattern resolver works with.
	 * @return
	 */
	public ClassLoader getClassLoader() {
		return classLoader;
	}
	
	/**
	 * Find all resources that match the given location pattern via the 
	 * Ant-style PathMatcher. Supports resources in jar files and zip files
	 * and in the file system.
	 * @param locationPattern
	 * @return
	 */
	protected Resource[] findPathMatchingResources(String locationPattern) throws IOException {
		String rootDirPath = determineRootDir(locationPattern);
		String subPattern = locationPattern.substring(rootDirPath.length());
		Resource[] rootDirResources = getResources(rootDirPath);
		Set<Resource> result = (Set<Resource>)CollectionFactory.createLinkedSetIfPossible(16);
		for (Resource resource : rootDirResources) {
			if (isJarResource(resource)) {
				result.addAll(doFindPathMatchingJarResources(resource, subPattern));
			} 
			else {
				result.addAll(doFindPathMatchingFileResources(resource, subPattern));
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Resolved location pattern [%s] to resources %s", locationPattern, result));
		}
		
		return (Resource[]) result.toArray(new Resource[result.size()]);
	}
	
	/**
	 * Determine the root directory for the given location.
	 * @param location
	 * @return
	 */
	protected String determineRootDir(String location) {
		int prefixIndex = location.indexOf(":") + 1;
		int rootDirIndex = location.length();
		while (rootDirIndex > prefixIndex && getPathMatcher().isPattern(location.substring(prefixIndex, rootDirIndex))) {
			rootDirIndex = location.lastIndexOf("/", rootDirIndex - 2) + 1;
		}
		if (rootDirIndex == 0) {
			rootDirIndex = prefixIndex;
		}
		return location.substring(0, rootDirIndex);
	}
	
	/**
	 * Convert the given URL as returned from the ClassLoader into a Resource object.
	 * @param url
	 * @return
	 */
	protected Resource convertClassLoaderURL(URL url) {
		return new UrlResource(url);
	}
	
	/**
	 * Return whether the given resource handle indicates a jar resource
	 * that the doFindPathMatchingJarResources method can handle.
	 * @param resource
	 * @return
	 * @throws IOException
	 */
	protected boolean isJarResource(Resource resource) throws IOException {
		String protocol = resource.getURL().getProtocol();
		return (URL_PROTOCOL_JAR.equals(protocol)
			|| URL_PROTOCOL_ZIP.equals(protocol)
			|| URL_PROTOCOL_WSJAR.equals(protocol));
	}
	
	/**
	 * Find all resources in jar files that match the given location pattern
	 * via the Ant-style PathMatcher.
	 * @param rootDirResource
	 * @param subPattern
	 * @return
	 * @throws IOException
	 */
	protected Set<Resource> doFindPathMatchingJarResources(Resource rootDirResource, String subPattern) throws IOException {
		URLConnection con = rootDirResource.getURL().openConnection();
		JarFile jarFile = null;
		String jarFileUrl = null;
		String rootEntryPath = null;
		
		if (con instanceof JarURLConnection) {
			JarURLConnection jarCon = (JarURLConnection) con;
			jarFile = jarCon.getJarFile();
			jarFileUrl = jarCon.getJarFileURL().toExternalForm();
			JarEntry jarEntry = jarCon.getJarEntry();
			rootEntryPath = (jarEntry != null ? jarEntry.getName() : "");
		} 
		else {
			String urlFile = rootDirResource.getURL().getFile();
			int index = urlFile.indexOf(JAR_URL_SEPARATOR);
			jarFileUrl = urlFile.substring(0, index);
			if (jarFileUrl.startsWith(ResourceUtils.FILE_URL_PREFIX)) {
				jarFileUrl = jarFileUrl.substring(ResourceUtils.FILE_URL_PREFIX.length());
			}
			jarFile = new JarFile(jarFileUrl);
			jarFileUrl = ResourceUtils.FILE_URL_PREFIX + jarFileUrl;
			rootEntryPath = urlFile.substring(index + JAR_URL_SEPARATOR.length());
		}
		
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Looking for matching resources in jar file [%s]", jarFileUrl));
		}
		if (!"".equals(rootEntryPath) && !rootEntryPath.endsWith("/")) {
			rootEntryPath = rootEntryPath + "/";
		}
		Set<Resource> result = (Set<Resource>)CollectionFactory.createLinkedSetIfPossible(8);
		for (Enumeration<JarEntry> entries  = jarFile.entries(); entries.hasMoreElements();) {
			JarEntry entry = entries.nextElement();
			String entryPath = entry.getName();
			if (entryPath.startsWith(rootEntryPath)) {
				String relativePath = entryPath.substring(rootEntryPath.length());
				if (getPathMatcher().match(subPattern, relativePath)) {
					result.add(rootDirResource.createRelative(relativePath));
				}
			}
		}
		return result;
	}
	
	/**
	 * Find all resources in the file system that match the given location pattern
	 * via the Ant style PathMatcher.
	 * @param rootDirResource
	 * @param subPattern
	 * @return
	 * @throws IOException
	 */
	protected Set<Resource> doFindPathMatchingFileResources(Resource rootDirResource, String subPattern) throws IOException {
		File rootDir = null;
		try {
			rootDir = rootDirResource.getFile().getAbsoluteFile();
		}
		catch (IOException ex) {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format(
					"Cannot search for matching files underneath %s because it does not correspond to a directory in the file system", rootDirResource), ex);
			}
			return Collections.EMPTY_SET;
		}
		return doFindMatchingFileSystemResources(rootDir, subPattern);
	}
	
	/**
	 * Find all resources in the file system that match the given location pattern
	 * via the Ant-style PathMatcher.
	 * @param rootDir
	 * @param subPattern
	 * @return
	 * @throws IOException
	 */
	protected Set<Resource> doFindMatchingFileSystemResources(File rootDir, String subPattern) throws IOException {
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Looking for matching resources in directory tree [%s]", rootDir.getPath()));
		}
		Set<File> matchingFiles = retrieveMatchingFiles(rootDir, subPattern);
		Set<Resource> result = (Set<Resource>)CollectionFactory.createLinkedSetIfPossible(matchingFiles.size());
		for (File file : matchingFiles) {
			result.add(new FileSystemResource(file));
		}
		return result;
	}
	
	/**
	 * Retrieve files that match the given path pattern,
	 * checking the given directory and its subdirectories.
	 * @param rootDir
	 * @param pattern
	 * @return
	 * @throws IOException
	 */
	protected Set<File> retrieveMatchingFiles(File rootDir, String pattern) throws IOException {
		if (!rootDir.isDirectory()) {
			throw new IllegalArgumentException(String.format("Resource path [%s] does not denote a directory", rootDir));
		}
		String fullPattern = StringUtils.replace(rootDir.getAbsolutePath(), File.separator, "/");
		if (!pattern.startsWith("/")) {
			fullPattern += "/";
		}
		fullPattern = fullPattern + StringUtils.replace(pattern, File.separator, "/");
		Set<File> result = (Set<File>)CollectionFactory.createLinkedSetIfPossible(8);
		doRetrieveMatchingFiles(fullPattern, rootDir, result);
		return result;
	}
	
	/**
	 * Recursively retrieve files that match the given pattern,
	 * adding them to the given result list.
	 * @param fullPattern
	 * @param dir
	 * @param result
	 * @throws IOException
	 */
	protected void doRetrieveMatchingFiles(String fullPattern, File dir, Set<File> result) throws IOException {
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Searching directory [%s] for files matching pattern [%s]", dir.getAbsolutePath(), fullPattern));
		}
		File[] dirContents = dir.listFiles();
		if (dirContents == null) {
			throw new IOException(String.format("Could not retrieve contents of directory", dir.getAbsolutePath()));
		}
		boolean dirDepthNotFixed = (fullPattern.indexOf("**") != -1);
		for (File file : dirContents) {
			String currPath = StringUtils.replace(file.getAbsolutePath(), File.separator, "/");
			if (file.isDirectory() 
			&& (dirDepthNotFixed || StringUtils.countOccurrencesOf(currPath, "/") < StringUtils.countOccurrencesOf(fullPattern, "/"))) {
				doRetrieveMatchingFiles(fullPattern, file, result);
			}
			if (getPathMatcher().match(fullPattern, currPath)) {
				result.add(file);
			}
		}
	}
	
	/**
	 * Find all class location resources with the given location via the ClassLoader.
	 * @param location
	 * @return
	 */
	protected Resource[] findAllClassPathResources(String location) throws IOException {
		String path = location;
		if (path.startsWith("/")) {
			path = path.substring(1);
		}
		Enumeration<URL> urls = getClassLoader().getResources(path);
		Set<Resource> result = (Set<Resource>)CollectionFactory.createLinkedSetIfPossible(16);
		while (urls.hasMoreElements()) {
			URL url = (URL)urls.nextElement();
			result.add(convertClassLoaderURL(url));
		}
		return (Resource[]) result.toArray(new Resource[result.size()]);
	}
	
	//----------------------------------------------------------------------------------------
	// Implementation of ResourcePatternResolver interface
	//----------------------------------------------------------------------------------------
	
	@Override
	public Resource getResource(String location) {
		return getResourceLoader().getResource(location);
	}

	@Override
	public Resource[] getResources(String locationPattern) throws IOException {
		Assert.notNull(locationPattern, "Location pattern must not be null");
		if (locationPattern.startsWith(CLASSPATH_ALL_URL_PREFIX)) {
			if (getPathMatcher().isPattern(locationPattern.substring(CLASSPATH_ALL_URL_PREFIX.length()))) {
				return findPathMatchingResources(locationPattern);
			} 
			else {
				return findAllClassPathResources(locationPattern.substring(CLASSPATH_ALL_URL_PREFIX.length()));
			}
		} 
		else {
			int index = locationPattern.indexOf(":") + 1;
			if (getPathMatcher().isPattern(locationPattern.substring(index))) {
				return findPathMatchingResources(locationPattern);
			} 
			else {
				return new Resource[] {getResourceLoader().getResource(locationPattern)};
			}
		}
		
	}

}
