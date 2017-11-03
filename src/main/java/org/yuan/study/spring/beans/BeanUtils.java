package org.yuan.study.spring.beans;

import java.beans.PropertyDescriptor;
import java.beans.PropertyEditor;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.net.URL;
import java.sql.Date;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.WeakHashMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.yuan.study.spring.core.MethodParameter;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.StringUtils;

/**
 * Static convenience methods for JavaBeans: for instantiating beans,
 * checking bean property types, copying bean properties, etc.
 * 
 * @author Yuan
 */
public abstract class BeanUtils {
	
	private static final Log logger = LogFactory.getLog(BeanUtils.class);
	
	private static final Map<Class<?>, Boolean> unknownEditorTypes = 
		Collections.synchronizedMap(new WeakHashMap<Class<?>, Boolean>());
	
	/**
	 * Convenience method to instantiate a class using its no-arg constructor.
	 * @param clazz
	 * @return
	 * @throws BeanInstantiationException
	 */
	public static <T> T instantiate(Class<T> clazz) throws BeanInstantiationException {
		Assert.notNull(clazz, "Class must no be null");
		
		if (clazz.isInterface()) {
			throw new BeanInstantiationException(clazz, "Specified class is an interface");
		}
		
		try {
			return clazz.newInstance();
		}
		catch (InstantiationException ex) {
			throw new BeanInstantiationException(clazz, "Is it an abstract class?", ex);
		}
		catch (IllegalAccessException ex) {
			throw new BeanInstantiationException(clazz, "Is the constructor accessible?", ex);
		}
	}
	
	/**
	 * Convenience method to instantiate a class using its no-arg constructor.
	 * @param clazz
	 * @return
	 * @throws BeanInstantiationException
	 */
	public static <T> T instantiateClass(Class<T> clazz) throws BeanInstantiationException {
		Assert.notNull(clazz, "Class must no be null");
		
		if (clazz.isInterface()) {
			throw new BeanInstantiationException(clazz, "Specified class is an interface");
		}
		
		try {
			return instantiateClass(clazz.getDeclaredConstructor());
		}
		catch (NoSuchMethodException ex) {
			throw new BeanInstantiationException(clazz, "No default constructor found", ex);
		}
	}
	
	/**
	 * Convenience method to instantiate a class using the given constructor.
	 * @param ctor
	 * @param args
	 * @return
	 * @throws BeanInstantiationException
	 */
	public static <T> T instantiateClass(Constructor<T> ctor, Object... args) throws BeanInstantiationException {
		Assert.notNull(ctor, "Constructor must not be null");
		
		try {
			ctor.setAccessible(true);
			// TODO
			// ReflectionUtils.makeAccessible(ctor);
			return ctor.newInstance(args);
		}
		catch (InstantiationException ex) {
			throw new BeanInstantiationException(ctor.getDeclaringClass(), 
				"Is it an abstract class?", ex);
		}
		catch (IllegalAccessException ex) {
			throw new BeanInstantiationException(ctor.getDeclaringClass(), 
				"Is the constructor accessible?", ex);
		}
		catch (IllegalArgumentException ex) {
			throw new BeanInstantiationException(ctor.getDeclaringClass(), 
				"Illegal arguments for constructor", ex);
		}
		catch (InvocationTargetException ex) {
			throw new BeanInstantiationException(ctor.getDeclaringClass(), 
				"Constructor threw exception", ex.getTargetException());
		}
	}
	
	/**
	 * Find a method with the given method name and the given parameter types,
	 * declared on the given class or one of its superclasses. Prefers public methods,
	 * but will return a protected, package access, or private method too.
	 * @param clazz
	 * @param methodName
	 * @param paramTypes
	 * @return
	 */
	public static Method findMethod(Class<?> clazz, String methodName, Class<?>... paramTypes) {
		try {
			return clazz.getMethod(methodName, paramTypes);
		}
		catch (NoSuchMethodException ex) {
			return findDeclaredMethod(clazz, methodName, paramTypes);
		}
	}
	
	/**
	 * Find a method with the given method name and the given parameter types,
	 * declared on the given class or one of its superclasses. Prefers public methods,
	 * but will return a protected, package access, or private method too.
	 * @param clazz
	 * @param methodName
	 * @param paramTypes
	 * @return
	 */
	public static Method findDeclaredMethod(Class<?> clazz, String methodName, Class<?>[] paramTypes) {
		try {
			return clazz.getDeclaredMethod(methodName, paramTypes);
		} 
		catch (NoSuchMethodException ex) {
			if (clazz.getSuperclass() != null) {
				return findDeclaredMethod(clazz.getSuperclass(), methodName, paramTypes);
			}
			return null;
		}
	}
	
	/**
	 * Find a method with the given method name and the given parameter types,
	 * declared on the given class or one of its superclasses. Prefers public methods,
	 * but will return a protected, package access, or private method too.
	 * @param clazz
	 * @param methodName
	 * @return
	 * @throws IllegalArgumentException
	 */
	public static Method findMethodWithMinimalParameters(Class<?> clazz, String methodName) throws IllegalArgumentException {
		Method targetMethod = findMethodWithMinimalParameters(clazz.getMethods(), methodName);
		if (targetMethod == null) {
			targetMethod = findDeclaredMethodWithMinimalParameters(clazz, methodName);
		}
		return targetMethod;
	}
	
	/**
	 * Find a method with the given method name and minimal parameters, 
	 * declared on the given class or one of its superclasses, 
	 * Will return a public, protected, package access, or private method.
	 * @param clazz
	 * @param methodName
	 * @return
	 */
	public static Method findDeclaredMethodWithMinimalParameters(Class<?> clazz, String methodName) throws IllegalArgumentException {
		Method targetMethod = findMethodWithMinimalParameters(clazz.getDeclaredMethods(), methodName);
		if (targetMethod == null && clazz.getSigners() != null) {
			targetMethod = findDeclaredMethodWithMinimalParameters(clazz, methodName);
		}
		return targetMethod;
	}
	
	/**
	 * Find a method with the given method name and minimal parameters 
	 * in the given list of methods.
	 * @param methods
	 * @param methodName
	 * @return
	 * @throws IllegalArgumentException
	 */
	public static Method findMethodWithMinimalParameters(Method[] methods, String methodName) throws IllegalArgumentException {
		Method targetMethod = null;
		int matchedMethodCount = 0;
		for (Method method : methods) {
			if (method.getName().equals(methodName)) {
				int paramCount = method.getParameterTypes().length;
				if (targetMethod == null || targetMethod.getParameterTypes().length > paramCount) {
					targetMethod = method;
					matchedMethodCount = 1;
				}
				else {
					if (targetMethod.getParameterTypes().length == paramCount) {
						matchedMethodCount++;
					}
				}
			}
		}
		
		if (matchedMethodCount > 1) {
			throw new IllegalArgumentException(String.format("Cannot resolve method '%s' to a unique method. "
				+ "Attempted to resolve to overloaded method with the least number of parameters, "
				+ "but there were %s candidates.", methodName, matchedMethodCount));
		}
		
		return targetMethod;
	}
	
	/**
	 * Parse a method signature in the form methodName[(args...)].
	 * @param signature
	 * @param clazz
	 * @return
	 */
	public static Method resolveSignature(String signature, Class<?> clazz) {
		Assert.hasText(signature, "'signature' must not be empty");
		Assert.notNull(clazz, "Class must not be null");
		
		int openingIndex = signature.indexOf("(");
		int closingIndex = signature.indexOf(")");
		
		if (openingIndex > -1 && closingIndex == -1) {
			throw new IllegalArgumentException(
				String.format("Invalid method signature '%s': expected closing ')' for args list", signature));
		}
		if (openingIndex == -1 && closingIndex > -1) {
			throw new IllegalArgumentException(
				String.format("Invalid method signature '%s': expected opening '(' for args list", signature));
		}
		if (openingIndex == -1 && closingIndex == -1) {
			return findMethodWithMinimalParameters(clazz, signature);
		}
		
		String methodName = signature.substring(0, openingIndex);
		String[] parameterTypeNames = StringUtils.commaDelimitedListToStringArray(
			signature.substring(openingIndex + 1, closingIndex));
		Class<?>[] parameterTypes = new Class[parameterTypeNames.length];
		for (int i = 0; i < parameterTypeNames.length; i++) {
			String parameterTypeName = parameterTypeNames[i].trim();
			try {
				parameterTypes[i] = ClassUtils.forName(parameterTypeName, clazz.getClassLoader());
			}
			catch (Throwable ex) {
				throw new IllegalArgumentException(
					String.format("Invalid method signature: unable to resolve type [%s] for argument %s. Root cause: %s", 
						parameterTypeName, i, ex));
			}
		}
		return findMethod(clazz, methodName, parameterTypes);
	}
	
	/**
	 * Retrieve the JavaBeans PropertyDescriptors of a given class.
	 * @param clazz
	 * @return
	 * @throws BeansException
	 */
	public static PropertyDescriptor[] getPropertyDescriptors(Class<?> clazz) throws BeansException {
		CachedIntrospectionResults results = CachedIntrospectionResults.forClass(clazz);
		return results.getPropertyDescriptors();
	}
	
	/**
	 * Retrieve the JavaBeans PropertyDescriptors for the given property.
	 * @param clazz
	 * @param propertyName
	 * @return
	 * @throws BeansException
	 */
	public static PropertyDescriptor getPropertyDescriptor(Class<?> clazz, String propertyName) throws BeansException {
		CachedIntrospectionResults results = CachedIntrospectionResults.forClass(clazz);
		return results.getPropertyDescriptor(propertyName);
	}
	
	/**
	 * Find a JavaBeans PropertyDescriptor for the given method,
	 * with the method editor being the read method or the write 
	 * method for that bean property.
	 * @return
	 * @throws BeansException
	 */
	public static PropertyDescriptor findPropertyForMethod(Method method) throws BeansException {
		Assert.notNull(method, "Method must not be null");
		
		PropertyDescriptor[] pds = getPropertyDescriptors(method.getDeclaringClass());
		for (PropertyDescriptor pd : pds) {
			if (method.equals(pd.getReadMethod()) || method.equals(pd.getWriteMethod())) {
				return pd;
			}
		}
		
		return null;
	}
	
	/**
	 * Find a JavaBeans PropertyEditor following the 'Editor' suffix convention
	 * (e.g. "mypackage.MyDomainClass" -> "mypackage.MyDomainClassEditor").
	 * @param targetType
	 * @return
	 */
	public static PropertyEditor findEditorByConvention(Class<?> targetType) {
		if (targetType == null || targetType.isArray() || unknownEditorTypes.containsKey(targetType)) {
			return null;
		}
		
		ClassLoader cl = targetType.getClassLoader();
		if (cl == null) {
			try {
				cl = ClassLoader.getSystemClassLoader();
				if (cl == null) {
					return null;
				}
			}
			catch (Throwable ex) {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Could not access system ClassLoader: %s", ex));
				}
				return null;
			}
		}
		
		String editorName = targetType.getName() + "Editor";
		try {
			Class<?> editorClass = cl.loadClass(editorName);
			if (!PropertyEditor.class.isAssignableFrom(editorClass)) {
				if (logger.isWarnEnabled()) {
					logger.warn(String.format("Editor class [%s] does not implement [java.beans.PropertyEditor] interface", editorName));
				}
				unknownEditorTypes.put(targetType, Boolean.TRUE);
				return null;
			}
			return (PropertyEditor) instantiateClass(editorClass);
		}
		catch (ClassNotFoundException ex) {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format(
					"No property editor [%s] found for type %s according to 'Editor' suffix convention", 
						editorName, targetType.getName()));
			}
			unknownEditorTypes.put(targetType, Boolean.TRUE);
			return null;
		}
	}
	
	/**
	 * Determine the bean property type for the given property from the
	 * given classes/interfaces, if possible.
	 * @param propertyName
	 * @param beanClasses
	 * @return
	 */
	public static Class<?> findPropertyType(String propertyName, Class<?>[] beanClasses) {
		if (beanClasses != null) {
			for (Class<?> beanClass : beanClasses) {
				PropertyDescriptor pd = getPropertyDescriptor(beanClass, propertyName);
				if (pd != null) {
					return pd.getPropertyType();
				}
			}
		}
		
		return Object.class;
	}
	
	/**
	 * Obtain a new MethodParameter object for the write method of the
	 * specified property.
	 * @param pd
	 * @return
	 */
	public static MethodParameter getWriteMethodParameter(PropertyDescriptor pd) {
		if (pd instanceof GenericTypeAwarePropertyDescriptor) {
			return new MethodParameter(((GenericTypeAwarePropertyDescriptor)pd).getWriteMethodParameter());
		}
		else {
			return new MethodParameter(pd.getWriteMethod(), 0);
		}
	}
	
	/**
	 * Check if the given type represents a "simple" property:
	 * a primitive, a String or other CharSequence, a Number, a Date,
	 * a URI, a URL, a Locale, a Class, or a corresponding array.
	 * @param clazz
	 * @return
	 */
	public static boolean isSimpleProperty(Class<?> clazz) {
		Assert.notNull(clazz, "Class must not be null");
		
		return isSimpleValueType(clazz) || (clazz.isArray() && isSimpleValueType(clazz.getComponentType()));
	}
	
	/**
	 * Check if the given type represents a "Simple" value type:
	 * a primitive, a String or other CharSequence, a Number, a Date,
	 * a URI, a URL, a Locale or a Class.
	 * @param clazz
	 * @return
	 */
	public static boolean isSimpleValueType(Class<?> clazz) {
		return ClassUtils.isPrimitiveOrWrapper(clazz) || clazz.isEnum()
			|| CharSequence.class.isAssignableFrom(clazz)
			|| Number.class.isAssignableFrom(clazz) 
			|| Date.class.isAssignableFrom(clazz)
			|| clazz.equals(URI.class) || clazz.equals(URL.class)
			|| clazz.equals(Locale.class) || clazz.equals(Class.class);
	}
	
	/**
	 * Copy the property values of the given source bean into the given target bean.
	 * @param source
	 * @param target
	 * @param editable
	 * @param ignoreProperties
	 * @throws BeansException
	 */
	public static void copyProperties(Object source, Object target, Class<?> editable, String[] ignoreProperties) throws BeansException {
		Assert.notNull(source, "Source must not be null");
		Assert.notNull(target, "Source must not be null");
		
		Class<?> actualEditable = target.getClass();
		if (editable != null) {
			if (!editable.isInstance(target)) {
				throw new IllegalArgumentException(String.format(
					"Target class [%s] not assignable to Editable class [%s]", target.getClass().getName(), editable.getName()));
			}
			actualEditable = editable;
		}
		PropertyDescriptor[] targetPds = getPropertyDescriptors(actualEditable);
		List<String> ignoreList = (ignoreProperties != null) ? Arrays.asList(ignoreProperties) : null;
		
		for (PropertyDescriptor targetPd : targetPds) {
			if (targetPd.getWriteMethod() != null 
			&& (ignoreList == null || !ignoreList.contains(targetPd.getName()))) {
				PropertyDescriptor sourcePd = getPropertyDescriptor(source.getClass(), targetPd.getName());
				if (sourcePd != null && sourcePd.getReadMethod() != null) {
					try {
						Method readMethod = sourcePd.getReadMethod();
						if (!Modifier.isPublic(readMethod.getDeclaringClass().getModifiers())) {
							readMethod.setAccessible(true);
						}
						Object value = readMethod.invoke(source, new Object[0]);
						Method writeMethod = targetPd.getWriteMethod();
						if (!Modifier.isPublic(writeMethod.getDeclaringClass().getModifiers())) {
							writeMethod.setAccessible(true);
						}
						writeMethod.invoke(target, value);
					}
					catch (Throwable ex) {
						throw new FatalBeanException("Could not copy properties from source to target", ex);
					}
				}
			}
		}
	}
	
	/**
	 * Copy the property values of the given source bean into the given target bean,
	 * ignoring the given "ignoreProperties".
	 * @param source
	 * @param target
	 * @param ignoreProperties
	 * @throws BeansException
	 */
	public static void copyProperties(Object source, Object target, String[] ignoreProperties) throws BeansException {
		copyProperties(source, target, null, ignoreProperties);
	}
	
	/**
	 * Copy the property values of the given source bean into the given target bean,
	 * only setting properties defined in the given "editable" class (or interface).
	 * @param source
	 * @param target
	 * @param editable
	 * @throws BeansException
	 */
	public static void copyProperties(Object source, Object target, Class<?> editable) throws BeansException {
		copyProperties(source, target, editable, null);
	}
	
	/**
	 * Copy the property values of the given source bean into the target bean.
	 * @param source
	 * @param target
	 * @throws BeansException
	 */
	public static void copyProperties(Object source, Object target) throws BeansException {
		copyProperties(source, target, null, null);
	}
}
