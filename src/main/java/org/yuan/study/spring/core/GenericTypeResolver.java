package org.yuan.study.spring.core;

import java.lang.ref.Reference;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;

import org.yuan.study.spring.util.Assert;

public abstract class GenericTypeResolver {
	
	private static final Map<Class, Reference<Map<TypeVariable, Type>>> typeVariableCache = 
		Collections.synchronizedMap(new WeakHashMap<Class, Reference<Map<TypeVariable, Type>>>());

	/**
	 * 
	 * @param methodParam
	 * @return
	 */
	public static Type getTargetType(MethodParameter methodParam) {
		Assert.notNull(methodParam, "MethodParameter must not be null");
		if (methodParam.getConstructor() != null) {
			return methodParam.getConstructor().getGenericParameterTypes()[methodParam.getParameterIndex()];
		}
		else {
			if (methodParam.getParameterIndex() >= 0) {
				return methodParam.getMethod().getGenericParameterTypes()[methodParam.getParameterIndex()];
			}
			else {
				return methodParam.getMethod().getGenericReturnType();
			}
		}
	}
	
	public static Class<?> resolveParameterType(MethodParameter methodParam, Class<?> clazz) {
		
	}
	
	public static Class<?> resolveReturnType(Method method, Class<?> clazz) {
		
	}
	
	public static Class<?> resolveTypeArgument(Class<?> clazz, Class<?> genericIfc) {
		
	}
	
	public static Class[] resolveTypeArgument(Class<?> clazz, Class<?> genericIfc) {
		
	}
	
	private static Class[] doResolveTypeArguments(Class<?> ownerClass, Class<?> classToIntrospect, Class<?> genericIfc) {
		
	}
	
	private static Class[] doResolveTypeArguments(Class<?> ownerClass, Type ifc, Class<?> genericIfc) {
		
	}
	
	private static Class<?> extractClass(Class<?> ownerClass, Type arg) {
		
	}
	
	/**
	 * Resolve the specified generic type against the given TypeVariable map.
	 * @param genericType
	 * @param typeVariableMap
	 * @return
	 */
	public static Class<?> resolveType(Type genericType, Map<TypeVariable, Type> typeVariableMap) {
		Type rawType = getRawType(genericType, typeVariableMap);
		return (rawType instanceof Class ? (Class<?>) rawType : Object.class);
	}
	
	/**
	 * 
	 * @param genericType
	 * @param typeVariableMap
	 * @return
	 */
	static Type getRawType(Type genericType, Map<TypeVariable, Type> typeVariableMap) {
		// TODO
		return null;
	}
	
	public static Map<TypeVariable, Type> getTypeVariableMap(Class<?> clazz) {
		
	}
	
	static Type extractBoundForTypeVariable(TypeVariable typeVariable) {
		
	}
	
	private static void extractTypeVariablesFromGenericInterfaces(Type[] genericInterfaces, Map<TypeVariable, Type> typeVariableMap) {
		
	}
	
	private static void populateTypeMapFromParameterizedType(ParameterizedType type, Map<TypeVariable, Type> typeVariableMap) {
		
	}
}
