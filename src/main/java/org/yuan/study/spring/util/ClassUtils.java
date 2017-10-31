package org.yuan.study.spring.util;

import java.beans.Introspector;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public abstract class ClassUtils {
	/** Suffix for array class names */
	public static final String ARRAY_SUFFIX = "[]";

	/** The ".class" file suffix */
	public static final String CLASS_FILE_SUFFIX = ".class";
	
	/** The CGLIB class separator character "$$" */
	public static final String CGLIB_CLASS_SEPARATOR = "$$";
	
	/** Prefix for internal array class names: "[" */
	private static final String INTERNAL_ARRAY_PREFIX = "[";
	
	/** Prefix for internal non-primitive array class names: "[L" */
	private static final String NON_PRIMITIVE_ARRAY_PREFIX = "[L";
	
	/** The package separator character '.' */
	private static final char PACKAGE_SEPARATOR = '.';
	
	/** The inner class separator character '$' */
	private static final char INNER_CLASS_SEPARATOR = '$';
	
	/**
	 * Map with primitive wrapper type as key and corresponding primitive
	 * type as value, for example: Integer.class -> int.class
	 */
	private static final Map<Class<?>, Class<?>> PRIMITIVE_WRAPPER_TYPE_MAP = new HashMap<Class<?>, Class<?>>(8);
	
	/**
	 * Map with primitive type as key and corresponding wrapper 
	 * type as value, for example: int.class -> Integer.class
	 */
	private static final Map<Class<?>, Class<?>> PRIMITIVE_TYPE_TO_WRAPPER_MAP = new HashMap<Class<?>, Class<?>>(8); 
	
	/**
	 * Map with primitive type name as key and corresponding primitivce
	 * type as value, for example: "int" -> int.class
	 */
	private static final Map<String, Class<?>> PRIMITIVE_TYPE_NAME_MAP = new HashMap<String, Class<?>>(32);
	
	/**
	 * Map with common "java.lang" class name as key and coresponding Class as value.
	 * Primarily for efficient deserialization of remote invocations.
	 */
	private static final Map<String, Class<?>> COMMON_CLASS_CACHE = new HashMap<String, Class<?>>(32);
	
	private static final Log logger = LogFactory.getLog(ClassUtils.class);

	
	static {
		PRIMITIVE_WRAPPER_TYPE_MAP.put(Boolean.class, boolean.class);
		PRIMITIVE_WRAPPER_TYPE_MAP.put(Byte.class, byte.class);
		PRIMITIVE_WRAPPER_TYPE_MAP.put(Character.class, char.class);
		PRIMITIVE_WRAPPER_TYPE_MAP.put(Double.class, double.class);
		PRIMITIVE_WRAPPER_TYPE_MAP.put(Float.class, float.class);
		PRIMITIVE_WRAPPER_TYPE_MAP.put(Integer.class, int.class);
		PRIMITIVE_WRAPPER_TYPE_MAP.put(Long.class, long.class);
		PRIMITIVE_WRAPPER_TYPE_MAP.put(Short.class, short.class);
		
		for (Map.Entry<Class<?>, Class<?>> entry : PRIMITIVE_WRAPPER_TYPE_MAP.entrySet()) {
			PRIMITIVE_TYPE_TO_WRAPPER_MAP.put(entry.getValue(), entry.getKey());
			registerCommonClasses(entry.getKey());
		}
		
		Set<Class<?>> primitiveTypes = new HashSet<Class<?>>(32);
		primitiveTypes.addAll(PRIMITIVE_WRAPPER_TYPE_MAP.values());
		primitiveTypes.addAll(Arrays.asList(
			boolean[].class, byte[].class, char[].class, double[].class,
			float[].class, int[].class, long[].class, short[].class));
		primitiveTypes.add(void.class);
		for (Class<?> primitiveType : primitiveTypes) {
			PRIMITIVE_TYPE_NAME_MAP.put(primitiveType.getName(), primitiveType);
		}
		
		registerCommonClasses(Boolean[].class, Byte[].class, Character[].class, Double[].class,
			Float[].class, Integer[].class, Long[].class, Short[].class);
		registerCommonClasses(Number.class, Number[].class, String.class, String[].class,
			Object.class, Object[].class, Class.class, Class[].class);
		registerCommonClasses(Throwable.class, Exception.class, RuntimeException.class,
			Error.class, StackTraceElement.class, StackTraceElement[].class);
	}
	
	/**
	 * Register the given common classes with the ClassUtils cache.
	 * @param commonClasses
	 */
	private static void registerCommonClasses(Class<?>... commonClasses) {
		for (Class<?> clazz : commonClasses) {
			COMMON_CLASS_CACHE.put(clazz.getName(), clazz);
		}
	}
	
	/**
	 * Return the default ClassLoader to use: typically the thread context
	 * ClassLoader, if available; the ClassLoader that loaded the ClassUtils
	 * class will be used as fallback.
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
	 * Override the thread context ClassLoader with the environment's bean ClassLoader
	 * if necessary, i.e. if the bean ClassLoader is not equivalent to the thread 
	 * context ClassLoader already.
	 * @param classLoaderToUse
	 * @return
	 */
	public static ClassLoader overrideThreadContextClassLoader(ClassLoader classLoaderToUse) {
		Thread currentThread = Thread.currentThread();
		ClassLoader threadContextClassLoader = currentThread.getContextClassLoader();
		if (classLoaderToUse != null && !classLoaderToUse.equals(threadContextClassLoader)) {
			currentThread.setContextClassLoader(classLoaderToUse);
			return threadContextClassLoader;
		}
		
		return null;
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
	 * Return teh qualified name of the given method, consisting of
	 * fully qualified interface/class name + "." + method name.
	 * @param method
	 * @return
	 */
	public static String getQualifiedMethodName(Method method) {
		Assert.notNull(method, "Method must not be null");
		
		return method.getDeclaringClass().getName() + "." + method.getName();
	}
	
	/**
	 * Return a descriptive name for the given object's type: usually simply
	 * the class name, but component type class name + "[]" for arrays,
	 * and an appended list of implemented interfaces for JDK proxies.
	 * @param value
	 * @return
	 */
	public static String getDescriptiveType(Object value) {
		if (value == null) {
			return null;
		}
		
		Class<?> clazz = value.getClass();
		if (Proxy.isProxyClass(clazz)) {
			StringBuilder sb = new StringBuilder(clazz.getName());
			sb.append(" implementing ");
			Class<?>[] ifcs = clazz.getInterfaces();
			for (int i = 0; i < ifcs.length; i++) {
				sb.append(ifcs[i].getName());
				if (i < ifcs.length - 1) {
					sb.append(",");
				}
			}
			return sb.toString();
		}
		
		if (clazz.isArray()) {
			return getQualifiedNameForArray(clazz);
		}
		
		return clazz.getName();
	}
	
	/**
	 * Check whether the given class matches the user-specified type name.
	 * @param clazz
	 * @param typeName
	 * @return
	 */
	public static boolean matchesTypeName(Class<?> clazz, String typeName) {
		if (typeName == null) {
			return false;
		}
		
		if (typeName.equals(clazz.getName())) {
			return true;
		}
		
		if (typeName.equals(clazz.getSimpleName())) {
			return true;
		}
		
		if (clazz.isArray() && typeName.equals(getQualifiedNameForArray(clazz))) {
			return true;
		}
		
		return false;
	}
	
	/**
	 * Determine whether the given class has a public constructor with the given signature.
	 * @param clazz
	 * @param paramTypes
	 * @return
	 */
	public static boolean hasConstructor(Class<?> clazz, Class<?>... paramTypes) {
		return (getConstructorIfAvailable(clazz, paramTypes) != null);
	}
	
	/**
	 * Determine whether the given class has a public constructor with the given signature,
	 * and return it if available (else return null).
	 * @param clazz
	 * @param paramTypes
	 * @return
	 */
	public static <T> Constructor<T> getConstructorIfAvailable(Class<T> clazz, Class<?>... paramTypes) {
		Assert.notNull(clazz, "Class must not be null");
		
		try {
			return clazz.getConstructor(paramTypes);
		}
		catch (NoSuchMethodException ex) {
			return null;
		}
	}
	
	
	
	/**
	 * Replacement for Class.forName() that also returns Class instances 
	 * for primitives (like "int") and array class names (like "String[]").
	 * @param name
	 * @return
	 * @throws ClassNotFoundException
	 * @throws LinkageError
	 */
	public static Class<?> forName(String name) throws ClassNotFoundException, LinkageError {
		return forName(name, getDefaultClassLoader());
	}
	
	/**
	 * Replacement for Class.forName() that also returns Class instances 
	 * for primitives (like "int") and array class names (like "String[]").
	 * @param name
	 * @return
	 * @throws ClassNotFoundException
	 * @throws LinkageError
	 */
	public static Class<?> forName(String name, ClassLoader classLoader) throws ClassNotFoundException, LinkageError {
		Assert.notNull(name, "Name must not be null");
		
		Class<?> clazz = resolvePrimitiveClassName(name);
		if (clazz == null) {
			clazz = COMMON_CLASS_CACHE.get(name);
		}
		if (clazz != null) {
			return clazz;
		}
		
		// java.lang.Object[]
		if (name.endsWith(ARRAY_SUFFIX)) {
			String elementName = name.substring(0, name.length() - ARRAY_SUFFIX.length());
			Class<?> elementClass = forName(elementName, classLoader);
			return Array.newInstance(elementClass, 0).getClass();
		}
		
		// [Ljava.lang.Object;
		if (name.startsWith(NON_PRIMITIVE_ARRAY_PREFIX) && name.endsWith(";")) {
			String elementName = name.substring(NON_PRIMITIVE_ARRAY_PREFIX.length(), name.length() - 1);
			Class<?> elementClass = forName(elementName, classLoader);
			return Array.newInstance(elementClass, 0).getClass();
		}
		
		// [[I or [[Ljava.lang.Object
		if (name.startsWith(INTERNAL_ARRAY_PREFIX)) {
			String elementName = name.substring(INTERNAL_ARRAY_PREFIX.length());
			Class<?> elementClass = forName(elementName, classLoader);
			return Array.newInstance(elementClass, 0).getClass();
		}
		
		ClassLoader classLoaderToUse = classLoader;
		if (classLoaderToUse == null) {
			classLoaderToUse = getDefaultClassLoader();
		}
		try {
			return classLoaderToUse.loadClass(name);
		} 
		catch (ClassNotFoundException ex) {
			int lastDotIndex = name.lastIndexOf('.');
			if (lastDotIndex != -1) {
				String innerClassName = name.substring(0, lastDotIndex) + INNER_CLASS_SEPARATOR + name.substring(lastDotIndex + 1);
				try {
					return classLoaderToUse.loadClass(innerClassName);
				} catch (ClassNotFoundException ex2) {}
			}
			throw ex;
		}
	}
	
	/**
	 * 
	 * @param className
	 * @param classLoader
	 * @return
	 * @throws IllegalArgumentException
	 */
	public static Class<?> resolveClassName(String className, ClassLoader classLoader) throws IllegalArgumentException {
		try {
			return forName(className, classLoader);
		} 
		catch (ClassNotFoundException ex) {
			throw new IllegalArgumentException(String.format("Cannot find class [%s]", className), ex);
		}
		catch (LinkageError ex) {
			throw new IllegalArgumentException(
				String.format("Error loading class [%s]: problem with class file or dependent class.", className), ex);
		}
	}
	
	/**
	 * Resolve the given class name as primitive class, if appropriate,
	 * according to the JVM's naming rules for primitive classes.
	 * @param name
	 * @return
	 */
	public static Class<?> resolvePrimitiveClassName(String name) {
		if (name.length() > 2 && name.length() <= 8) {
			return PRIMITIVE_TYPE_NAME_MAP.get(name);
		}
		return null;
	}
	
	/**
	 * Return the number of methods with a given name (with any argument types),
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
	public static boolean hasMethod(Class<?> clazz, String methodName, Class<?>... paramTypes) {
		return (getMethodIfAvailable(clazz, methodName, paramTypes) != null);
	}
	
	/**
	 * Determine whether the given class has a method with the given signature,
	 * and return it if available (else return null).
	 * @param clazz
	 * @param methodName
	 * @param paramTypes
	 * @return
	 */
	public static Method getMethodIfAvailable(Class<?> clazz, String methodName, Class<?>... paramTypes) {
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
	 * Does the given class or one of its superclasses at least have
	 * one or more method with the supplied name (with any argument types)?
	 * Includes non-public methods.
	 * @param clazz
	 * @param methodName
	 * @return
	 */
	public static boolean hasAtLeastOneMethodWithName(Class<?> clazz, String methodName) {
		Assert.notNull(clazz, "Class must not be null");
		Assert.notNull(methodName, "Method name must not be null");
		
		for (Method method : clazz.getDeclaredMethods()) {
			if (methodName.equals(method.getName())) {
				return true;
			}
		}
		
		for (Class<?> ifc : clazz.getInterfaces()) {
			if (hasAtLeastOneMethodWithName(ifc, methodName)) {
				return true;
			}
		}
		
		if (clazz.getSuperclass() != null 
			&& hasAtLeastOneMethodWithName(clazz, methodName)) {
			return true;
		}
		
		return false;
	}
	
	/**
	 * Given a method, which may come from an interface, and a target 
	 * class used in the current reflective invocation, find the 
	 * corresponding target method if there is one. E.g. the method 
	 * may be IFoo.bar() and the target class may be DefaultFoo, 
	 * In thsi case, the method may be DefaultFoo.bar().
	 * @param method
	 * @param targetClass
	 * @return
	 */
	public static Method getMostSpecificMethod(Method method, Class<?> targetClass) {
		Method specificMethod = null;
		if (method != null && isOverridable(method, targetClass) 
			&& targetClass != null && !targetClass.equals(method.getDeclaringClass())) {
			specificMethod = ReflectionUtils.findMethod(targetClass, method.getName(), method.getParameterTypes());
		}
		return (specificMethod != null ? specificMethod : method);
	}
	
	/**
	 * Determine whether the given method is overridable in the given target class.
	 * @param method
	 * @param targetClass
	 * @return
	 */
	private static boolean isOverridable(Method method, Class<?> targetClass) {
		if (Modifier.isPrivate(method.getModifiers())) {
			return false;
		}
		
		if (Modifier.isPublic(method.getModifiers()) || Modifier.isProtected(method.getModifiers())) {
			return true;
		}
		
		if (getPackageName(method.getDeclaringClass()).equals(getPackageName(targetClass))) {
			return true;
		}
		
		return false;
	}

	/**
	 * return a public static method of a class.
	 * @param clazz
	 * @param methodName
	 * @param paramType
	 * @return
	 */
	public static Method getStaticMethod(Class<?> clazz, String methodName, Class<?>... paramType) {
		Assert.notNull(clazz, "Class must not be null");
		Assert.notNull(methodName, "Method name must not be null");
		
		try {
			Method method = clazz.getMethod(methodName, paramType);
			return Modifier.isStatic(method.getModifiers()) ? method : null;
		} catch (NoSuchMethodException ex) {
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
	 * Determine whether the Class identified by the supplied name is present
	 * and can be loaded. Will return false if either the class or one of its 
	 * dependencies is not present or cannot be loaded.
	 * @param str
	 * @param classLoader
	 * @return
	 */
	public static boolean isPresent(String className) {
		return isPresent(className, getDefaultClassLoader());
	}
	
	/**
	 * Determine whether the Class identified by the supplied name is present
	 * and can be loaded. Will return false if either the class or one of its 
	 * dependencies is not present or cannot be loaded.
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
	 * Return the user-defined class for the given instance: usually simply
	 * the class of the given instance, but the original class in case of 
	 * a CGLIB-generated subclass.
	 * @return
	 */
	public static Class<?> getUserClass(Object instance) {
		Assert.notNull(instance, "Instance must not be null");
		
		return getUserClass(instance.getClass());
	}
	
	/**
	 * Return the user-defined class for the given class: usually simply
	 * the class, but the original class in case of a CGLIB-generated subclass.
	 * @param clazz
	 * @return
	 */
	public static Class<?> getUserClass(Class<?> clazz) {
		if (clazz != null && clazz.getName().contains(CGLIB_CLASS_SEPARATOR)) {
			Class<?> superClass = clazz.getSuperclass();
			if (superClass != null && !Object.class.equals(superClass)) {
				return superClass;
			}
		}
		
		return clazz;
	}
	
	/**
	 * Return the short string name of a Java class in uncapitalized JavaBeans
	 * property format. Strips the outer class name in case of an inner class.
	 * @param clazz
	 * @return
	 */
	public static String getShortNameAsProperty(Class<?> clazz) {
		String shortName = getShortName(clazz);
		int dotIndex = shortName.lastIndexOf(PACKAGE_SEPARATOR);
		shortName = (dotIndex != -1 ? shortName.substring(dotIndex + 1) : shortName);
		return Introspector.decapitalize(shortName);
	}
	
	/**
	 * Determine the name of the class file, relative to the containing 
	 * package: e.g. "String.class"
	 * @param clazz
	 * @return
	 */
	public static String getClassFileName(Class<?> clazz) {
		Assert.notNull(clazz, "Class must not be null");
		
		String className = clazz.getName();
		int lastDotIndex = className.lastIndexOf(PACKAGE_SEPARATOR);
		return className.substring(lastDotIndex + 1) + CLASS_FILE_SUFFIX;
	}
	
	/**
	 * Determine the name of the package of the given class:
	 * e.g. "java.lang" for the java.lang.String class.
	 * @param clazz
	 * @return
	 */
	public static String getPackageName(Class<?> clazz) {
		Assert.notNull(clazz, "Class must not be null");
		
		String className = clazz.getName();
		int lastDotIndex = className.lastIndexOf(PACKAGE_SEPARATOR);
		return (lastDotIndex != -1 ? className.substring(0, lastDotIndex) : "");
	}
	
	/**
	 * Check if the given class represents a primitive wrapper.
	 * @param clazz
	 * @return
	 */
	public static boolean isPrimitiveWrapper(Class<?> clazz) {
		Assert.notNull(clazz, "Class must not be null");
		
		return PRIMITIVE_WRAPPER_TYPE_MAP.containsKey(clazz);
	}
	
	/**
	 * Check if the given class represents a primitive or a primitive wrapper.
	 * @param clazz
	 * @return
	 */
	public static boolean isPrimitiveOrWrapper(Class<?> clazz) {
		Assert.notNull(clazz, "Class must not be null");
		
		return (clazz.isPrimitive() || isPrimitiveWrapper(clazz));
	}
	
	/**
	 * Check if the given class represents an array of primitives.
	 * @param clazz
	 * @return
	 */
	public static boolean isPrimitiveArray(Class<?> clazz) {
		Assert.notNull(clazz, "Class must not be null");
		
		return (clazz.isArray() || clazz.getComponentType().isPrimitive());
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
