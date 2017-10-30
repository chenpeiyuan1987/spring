package org.yuan.study.spring.util;

import java.lang.reflect.Array;
import java.lang.reflect.Method;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public abstract class ClassUtils {
	/** Suffix for array class names */
	public static final String ARRAY_SUFFIX = "[]";
	
	/** All primitive classes */
	private static Class<?>[] PRIMITIVE_CLASSES = {
		boolean.class, byte.class, char.class, short.class, 
		int.class, long.class, float.class, double.class, void.class
	};
	
	/** The package separator character '.' */
	private static final char PACKAGE_SEPARATOR = '.';
	
	/** The inner class separator character '$' */
	private static final char INNER_CLASS_SEPARATOR = '$';
	
	/** The CGLIB class separator character "$$" */
	private static final String CGLIB_CLASS_SEPARATOR = "$$";
	
	private static final Log logger = LogFactory.getLog(ClassUtils.class);

	/**
	 * Return a default ClassLoader to use.
	 * @return
	 */
	public static ClassLoader getDefaultClassLoader() {
		ClassLoader cl = null;
		
		try {
			cl = Thread.currentThread().getContextClassLoader();
		}
		catch (Exception ex) {
			logger.debug("Cannot access thread context ClassLoader - falling back to system class loader", ex);
		}
		if (cl == null) {
			cl = ClassUtils.class.getClassLoader();
		}
		
		return cl;
	}
	
	/**
	 * Return the qualified name of the given class: usually simply the class name,
	 * but component type class name + "[]" for arrays.
	 * @param clazz
	 * @return
	 */
	public static String getQualifiedName(Class<?> clazz) {
		Assert.notNull(clazz, "Class must not be null");
		if (clazz.isArray()) {
			return getQualifiedNameForArray(clazz);
		}
		else {
			return clazz.getName();
		}
	}
	
	/**
	 * Build a nice qualified name for an array:
	 * component type class name + "[]".
	 * @param clazz
	 * @return
	 */
	public static String getQualifiedNameForArray(Class<?> clazz) {
		StringBuffer sb = new StringBuffer();
		while (clazz.isArray()) {
			sb.append(ClassUtils.ARRAY_SUFFIX);
			clazz = clazz.getComponentType();
		}
		sb.insert(0, clazz.getName());
		return sb.toString();
	}
	
	/**
	 * Replacement for Class.forName() that also returns Class instances for primitives (like "int") and array class names (like "String[]").
	 * @param name
	 * @return
	 * @throws ClassNotFoundException
	 * @throws LinkageError
	 */
	public static Class<?> forName(String name) throws ClassNotFoundException, LinkageError {
		return forName(name, getDefaultClassLoader());
	}
	
	/**
	 * Replacement for Class.forName() that also returns Class instances for primitives (like "int") and array class names (like "String[]").
	 * @param name
	 * @param classLoader
	 * @return
	 * @throws ClassNotFoundException
	 * @throws LinkageError
	 */
	public static Class<?> forName(String name, ClassLoader classLoader) throws ClassNotFoundException, LinkageError {
		Assert.notNull(name, "Name must not be null");
		Class<?> clazz = resolvePrimitiveClassName(name);
		if (clazz != null) {
			return clazz;
		}
		if (name.endsWith(ARRAY_SUFFIX)) {
			String elementClassName = name.substring(0, name.length() - ARRAY_SUFFIX.length());
			Class<?> elementClass = forName(elementClassName, classLoader);
			return Array.newInstance(elementClass, 0).getClass();
		}
		return Class.forName(name, true, classLoader);
	}
	
	/**
	 * Resolve the given class name as primitive class, if appropriate.
	 * @param name
	 * @return
	 */
	public static Class<?> resolvePrimitiveClassName(String name) {
		if (name.length() > 2 && name.length() < 8) {
			for (Class<?> clazz : PRIMITIVE_CLASSES) {
				if (clazz.getName().equals(name)) {
					return clazz;
				}
			}
		}
		return null;
	}
	
	/**
	 * Return the number of methods with a given name,
	 * for the given class and/or its superclasses. Includes non-public methods.
	 * @param clazz
	 * @param methodName
	 * @return
	 */
	public static int getMethodCountForName(Class<?> clazz, String methodName) {
		Assert.notNull(clazz, "Class must not be null");
		Assert.notNull(methodName, "Method name must not be null");
		int count = 0;
		for (Method method : clazz.getDeclaredMethods()) {
			if (methodName.equals(method.getName())) {
				count++;
			}
		}
		Class<?>[] ifcs = clazz.getInterfaces();
		for (Class<?> ifc : ifcs) {
			count += getMethodCountForName(ifc, methodName);
		}
		if (clazz.getSuperclass() != null) {
			count += getMethodCountForName(clazz.getSuperclass(), methodName);
		}
		return count;
	}
	
	/**
	 * Determine whether the given class has a method with the given signature.
	 * @param clazz
	 * @param methodName
	 * @param paramTypes
	 * @return
	 */
	public static boolean hasMethod(Class<?> clazz, String methodName, Class<?>[] paramTypes) {
		return (getMethodIfAvailable(clazz, methodName, paramTypes) != null);
	}
	
	/**
	 * Determine whether the given class has a method with the given signature,
	 * and return it if available.
	 * @param clazz
	 * @param methodName
	 * @param paramTypes
	 * @return
	 */
	public static Method getMethodIfAvailable(Class<?> clazz, String methodName, Class<?>[] paramTypes) {
		Assert.notNull(clazz, "Class must not be null");
		Assert.notNull(methodName, "Method name must not be null");
		try {
			return clazz.getMethod(methodName, paramTypes);
		}
		catch (NoSuchMethodException ex) {
			return null;
		}
	}

	/**
	 * Give an input class object, return a string which consists of 
	 * the class's package name as a pathname, i.e., all dots('.') 
	 * are replaced by slashes('/').
	 * @param clazz
	 * @return
	 */
	public static String classPackageAsResourcePath(Class<?> clazz) {
		if (clazz == null || clazz.getPackage() == null) {
			return "";
		}
		return clazz.getPackage().getName().replace(".", "/");
	}
	
	/**
	 * Get the class name without the qualified package name.
	 * @param clazz
	 * @return
	 */
	public static String getShortName(Class<?> clazz) {
		return getShortName(getQualifiedName(clazz));
	}
	
	/**
	 * Get the class name without the qualified package name.
	 * @param className
	 * @return
	 */
	public static String getShortName(String className) {
		Assert.hasLength(className, "Class name must not be empty");
		int lastDotIndex = className.lastIndexOf(PACKAGE_SEPARATOR);
		int nameEndIndex = className.indexOf(CGLIB_CLASS_SEPARATOR);
		if (nameEndIndex == -1) {
			nameEndIndex = className.length();
		}
		String shortName = className.substring(lastDotIndex + 1, nameEndIndex);
		shortName = shortName.replace(INNER_CLASS_SEPARATOR, PACKAGE_SEPARATOR);
		return shortName;
	}
	
	/**
	 * Check whether the given class it cache-safe in the given context,
	 * i.e. whether it is loaded by the given ClassLoader or a parent of it.
	 * @param clazz
	 * @param classLoader
	 * @return
	 */
	public static boolean isCacheSafe(Class<?> clazz, ClassLoader classLoader) {
		Assert.notNull(clazz, "Class must not be null");
		
		ClassLoader target = clazz.getClassLoader();
		if (target == null) {
			return false;
		}
		
		ClassLoader loader = classLoader;
		if (loader == target) {
			return true;
		}
		while (loader != null) {
			loader = loader.getParent();
			if (loader == target) {
				return true;
			}
		}
		
		return false;
	}
	
	/**
	 * Determine whether the identified by the supplied name is present
	 * and can be loaded. Will return false if either the class or one.
	 * @param str
	 * @param classLoader
	 * @return
	 */
	public static boolean isPresent(String className, ClassLoader classLoader) {
		try {
			forName(className, classLoader);
			return true;
		} catch (Throwable ex) {
			return false;
		}
	}
	
	/**
	 * 
	 * @param clazz
	 * @return
	 */
	public static boolean isPrimitiveOrWrapper(Class<?> clazz) {
		// TODO
		return false;
	}
	
	/**
	 * 
	 * @param clazz
	 * @param classLoader
	 * @return
	 */
	public static boolean isVisible(Class<?> clazz, ClassLoader classLoader) {
		if (classLoader == null) {
			return true;
		}
		
		try {
			Class<?> actualClass = classLoader.loadClass(clazz.getName());
			return (clazz == actualClass);
		}
		catch (ClassNotFoundException ex) {
			return false;
		}
	}
}
