package org.yuan.study.spring.core;

import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.Map;

import org.yuan.study.spring.util.Assert;

public abstract class GenericTypeResolver {

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
	
	/**
	 * 
	 * @param method
	 * @param clazz
	 * @return
	 */
	public static Class<?> resolveReturnType(Method method, Class<?> clazz) {
		// TODO
		return null;
	}
}
