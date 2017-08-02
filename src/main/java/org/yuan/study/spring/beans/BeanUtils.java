package org.yuan.study.spring.beans;

import java.lang.reflect.Method;
import java.util.HashMap;
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
}
