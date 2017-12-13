package org.yuan.study.spring.core;

import java.util.HashSet;
import java.util.Set;

import org.yuan.study.spring.util.Assert;

public class DecoratingClassLoader extends ClassLoader {

	private final Set<String> excludedPackages = new HashSet<String>();
	
	private final Set<String> excludedClasses = new HashSet<String>();
	
	private final Object exclusionMonitor = new Object();

	/**
	 * Create a new DecoratingClassLoader with no parent ClassLoader.
	 */
	public DecoratingClassLoader() {
	}

	/**
	 * Create a new DecoratingClassLoader with no parent ClassLoader.
	 * @param parent
	 */
	public DecoratingClassLoader(ClassLoader parent) {
		super(parent);
	}
	
	/**
	 * Add a package name to exclude from decoration
	 * @param packageName
	 */
	public void excludePackage(String packageName) {
		Assert.notNull(packageName, "Package name must not be null");
	
		synchronized (exclusionMonitor) {
			this.excludedPackages.add(packageName);
		}
	}
	
	/**
	 * Add a class name to exclude from decoration.
	 * @param className
	 */
	public void excludeClass(String className) {
		Assert.notNull(className, "Class name must not be null");
		
		synchronized (exclusionMonitor) {
			excludedClasses.add(className);
		}
	}
	
	/**
	 * Determine whether the specified class is excluded from decoration
	 * by thsi class loader.
	 * @param className
	 * @return
	 */
	protected boolean isExcluded(String className) {
		synchronized (exclusionMonitor) {
			if (excludedClasses.contains(className)) {
				return true;
			}
			for (String packageName : excludedPackages) {
				if (className.startsWith(packageName)) {
					return true;
				}
			}
		}
		return false;
	}
	
}
