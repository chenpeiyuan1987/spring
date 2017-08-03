package org.yuan.study.spring.util;

import java.lang.reflect.Array;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public abstract class ClassUtils {
	/** Suffix for array class names */
	public static final String ARRAY_SUFFIX = "[]";
	
	/** All primitive classes */
	private static Class<?>[] PRIMITIVE_CLASSES = {
		boolean.class, byte.class, char.class, short.class, 
		int.class, long.class, float.class, double.class
	};
	
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
	
}
