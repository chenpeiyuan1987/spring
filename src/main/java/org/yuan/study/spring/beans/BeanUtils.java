package org.yuan.study.spring.beans;

import java.beans.PropertyDescriptor;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.yuan.study.spring.util.Assert;

public class BeanUtils {
	
	private static final Map<Class<?>,Class<?>> primitiveWrapperTypeMap = new HashMap<Class<?>,Class<?>>(8);
	
	static {
		primitiveWrapperTypeMap.put(Boolean.class, boolean.class);
		primitiveWrapperTypeMap.put(Byte.class, byte.class);
		primitiveWrapperTypeMap.put(Character.class, char.class);
		primitiveWrapperTypeMap.put(Double.class, double.class);
		primitiveWrapperTypeMap.put(Float.class, float.class);
		primitiveWrapperTypeMap.put(Integer.class, int.class);
		primitiveWrapperTypeMap.put(Long.class, long.class);
		primitiveWrapperTypeMap.put(Short.class, short.class);
	}
	
	/**
	 * Find a method with the given method name and minimal parameters, 
	 * declared on the given class or one of its superclasses, 
	 * Will return a public, protected, package access, or private method.
	 * @param clazz
	 * @param methodName
	 * @return
	 */
	public static Method findDeclaredMethodWithMinimalParameters(Class<?> clazz, String methodName) {
		Method[] methods = clazz.getDeclaredMethods();
		Method targetMethod = null;
		for (Method method : methods) {
			if (method.getName().equals(methodName)) {
				if (targetMethod == null 
					|| targetMethod.getParameterTypes().length > method.getParameterTypes().length) {
					targetMethod = method;
				}
			}
		}
		if (targetMethod != null) {
			return targetMethod;
		}
		else {
			if (clazz.getSuperclass() != null) {
				return findDeclaredMethodWithMinimalParameters(clazz.getSuperclass(), methodName);
			}
			else {
				return null;
			}
		}
	}
	
	/**
	 * Check if the given class represents a "simple" property,
	 * i.e. a primitive, a String, a Class, or a corresponding array.
	 * @param clazz
	 * @return
	 */
	public static boolean isSimpleProperty(Class<?> clazz) {
		Assert.notNull(clazz, "clazz must not be null");
		return clazz.isPrimitive() || isPrimitiveArray(clazz)
			|| isPrimitiveWrapper(clazz) || isPrimitiveWrapperArray(clazz)
			|| clazz.equals(String.class) || clazz.equals(String[].class)
			|| clazz.equals(Class.class) || clazz.equals(Class[].class);
	}
	
	/**
	 * Check if the given class represents an array of primitives,
	 * i.e. boolean, byte, char, short, int, long, float, or double.
	 * @param clazz
	 * @return
	 */
	public static boolean isPrimitiveArray(Class<?> clazz) {
		return (clazz.isArray() && clazz.getComponentType().isPrimitive());
	}
	
	/**
	 * Check if the given class represents a primitive wrapper,
	 * i.e. Boolean, Byte, Character, Short, Integer, Long, Float, or Double.
	 * @param clazz
	 * @return
	 */
	public static boolean isPrimitiveWrapper(Class<?> clazz) {
		return primitiveWrapperTypeMap.get(clazz) != null;
	}
	
	/**
	 * Check if the given class represents an array of primitive wrappers,
	 * i.e. Boolean, Byte, Character, Short, Integer, Long, Float, or Double.
	 * @param clazz
	 * @return
	 */
	public static boolean isPrimitiveWrapperArray(Class<?> clazz) {
		return (clazz.isArray() && isPrimitiveWrapper(clazz.getComponentType()));
	}
	
	/**
	 * Determine if the given target type is assignable from the given value type, assuming setting by reflection. 
	 * Considers primitive  wrapper classes as assignable to the corresponding primitive types.
	 * @param targetType
	 * @param valueType
	 * @return
	 */
	public static boolean isAssignable(Class<?> targetType, Class<?> valueType) {
		Assert.notNull(targetType, "targetType must not be null");
		Assert.notNull(valueType, "valueType must not be null");
		return (targetType.isAssignableFrom(valueType) 
			|| targetType.equals(primitiveWrapperTypeMap.get(valueType)));
	}
	
	/**
	 * Determine if the given type is assignable from the given value, assuming setting by reflection. 
	 * Considers primitive wrapper classes as assignable to the corresponding primitive types.
	 * @param type
	 * @param value
	 * @return
	 */
	public static boolean isAssignable(Class<?> type, Object value) {
		Assert.notNull(type, "type must not be null");
		return (value != null ? isAssignable(type, value.getClass()) : !type.isPrimitive());
	}
	
	/**
	 * 
	 * @param clazz
	 * @return
	 */
	public static Object instantiateClass(Class<?> clazz) throws BeanInstantiationException {
		Assert.notNull(clazz, "Class must not be null");
		if (clazz.isInterface()) {
			throw new BeanInstantiationException(clazz, "Specified class is an interface");
		}
		try {
			return instantiateClass(clazz.getDeclaredConstructor((Class[]) null), null);
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
	public static Object instantiateClass(Constructor<?> ctor, Object[] args) throws BeanInstantiationException {
		Assert.notNull(ctor, "Constructor must not be null");
		try {
			if (!Modifier.isPublic(ctor.getModifiers()) 
				|| !Modifier.isPublic(ctor.getDeclaringClass().getModifiers())) {
				ctor.setAccessible(true);
			}
			return ctor.newInstance(args);
		}
		catch (InstantiationException ex) {
			throw new BeanInstantiationException(ctor.getDeclaringClass(),
				"Is it an abstract class?", ex);
		}
		catch (IllegalAccessException ex) {
			throw new BeanInstantiationException(ctor.getDeclaringClass(),
				"Has the class definition changed? Is the constructor accessible?", ex);
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
	 * 
	 * @param clazz
	 * @param methodName
	 * @param paramTypes
	 * @return
	 */
	public static Method findMethod(Class<?> clazz, String methodName, Class<?>[] paramTypes) {
		try {
			return clazz.getMethod(methodName, paramTypes);
		}
		catch (NoSuchMethodException ex) {
			return findDeclaredMethod(clazz, methodName, paramTypes);
		}
	}
	
	/**
	 * 
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
	 * Retrieve the JavaBeans PropertyDescriptors of a given class.
	 * @param clazz
	 * @return
	 * @throws BeansException
	 */
	public static PropertyDescriptor[] getPropertyDescriptors(Class<?> clazz) throws BeansException {
		CachedIntrospectionResults results = CachedIntrospectionResults.forClass(clazz);
		return results.getBeanInfo().getPropertyDescriptors();
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
	 * with the method either being the read method or the write 
	 * method for that bean property.
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
						writeMethod.invoke(target, new Object[] {value});
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
