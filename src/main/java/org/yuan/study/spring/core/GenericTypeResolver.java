package org.yuan.study.spring.core;

import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.Collections;
import java.util.HashMap;
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
		Assert.notNull(clazz, "Class must not be null");

		Type genericType = getTargetType(methodParam);
		Map<TypeVariable, Type> typeVariableMap = getTypeVariableMap(clazz);
		Type rawType = getRawType(genericType, typeVariableMap);
		Class<?> result = (rawType instanceof Class ? (Class<?>) rawType : methodParam.getParameterType());
		methodParam.setParameterType(result);
		methodParam.typeVariableMap = typeVariableMap;
		return result;
	}
	
	public static Class<?> resolveReturnType(Method method, Class<?> clazz) {
		Assert.notNull(method, "Method must not be null");
		Assert.notNull(clazz, "Class must not be null");
		
		Type genericType = method.getGenericReturnType();
		Map<TypeVariable, Type> typeVariableMap = getTypeVariableMap(clazz);
		Type rawType = getRawType(genericType, typeVariableMap);
		return (rawType instanceof Class ? (Class) rawType : method.getReturnType());
	}
	
	public static Class<?> resolveTypeArgument(Class<?> clazz, Class<?> genericIfc) {
		Class[] typeArgs = resolveTypeArguments(clazz, genericIfc);
		if (typeArgs == null) {
			return null;
		}
		if (typeArgs.length != 1) {
			throw new IllegalArgumentException(String.format(
				"Expected 1 type argument on generic interface [%s] but found %s", genericIfc.getName(), typeArgs.length));
		}
		return typeArgs[0];
	}
	
	public static Class[] resolveTypeArguments(Class<?> clazz, Class<?> genericIfc) {
		return doResolveTypeArguments(clazz, clazz, genericIfc);
	}
	
	private static Class[] doResolveTypeArguments(Class<?> ownerClass, Class<?> classToIntrospect, Class<?> genericIfc) {
		while (classToIntrospect != null) {
			if (genericIfc.isInterface()) {
				Type[] ifcs = classToIntrospect.getGenericInterfaces();
				for (Type ifc : ifcs) {
					Class[] result = doResolveTypeArguments(ownerClass, ifc, genericIfc);
					if (result != null) {
						return result;
					}
				}
			} 
			else {
				Class[] result = doResolveTypeArguments(
					ownerClass, classToIntrospect.getGenericSuperclass(), genericIfc);
				if (result != null) {
					return result;
				}
			}
			classToIntrospect = classToIntrospect.getSuperclass();
		}
		return null;
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
		Type resolvedType = genericType;
		if (genericType instanceof TypeVariable) {
			TypeVariable tv = (TypeVariable) genericType;
			resolvedType = typeVariableMap.get(tv);
			if (resolvedType == null) {
				resolvedType = extractBoundForTypeVariable(tv);
			}
		}
		if (resolvedType instanceof ParameterizedType) {
			return ((ParameterizedType) resolvedType).getRawType();
		}
		return resolvedType;
	}
	
	public static Map<TypeVariable, Type> getTypeVariableMap(Class<?> clazz) {
		Reference<Map<TypeVariable, Type>> ref = typeVariableCache.get(clazz);
		Map<TypeVariable, Type> typeVariableMap = (ref != null ? ref.get() : null);
		
		if (typeVariableMap == null) {
			typeVariableMap = new HashMap<TypeVariable, Type>();
			
			extractTypeVariablesFromGenericInterfaces(clazz.getGenericInterfaces(), typeVariableMap);
		
			Type genericType = clazz.getGenericSuperclass();
			Class<?> type = clazz.getSuperclass();
			while (type != null && !Object.class.equals(type)) {
				if (genericType instanceof ParameterizedType) {
					ParameterizedType pt = (ParameterizedType) genericType;
					populateTypeMapFromParameterizedType(pt, typeVariableMap);
				}
				extractTypeVariablesFromGenericInterfaces(type.getGenericInterfaces(), typeVariableMap);
				genericType = type.getGenericSuperclass();
				type = type.getSuperclass();
			}
			
			type = clazz;
			while (type.isMemberClass()) {
				genericType = type.getGenericSuperclass();
				if (genericType instanceof ParameterizedType) {
					ParameterizedType pt = (ParameterizedType) genericType;
					populateTypeMapFromParameterizedType(pt, typeVariableMap);
				}
				type = type.getEnclosingClass();
			}
			
			typeVariableCache.put(clazz, new WeakReference<Map<TypeVariable,Type>>(typeVariableMap));
		}
		
		return typeVariableMap;
	}
	
	static Type extractBoundForTypeVariable(TypeVariable typeVariable) {
		Type[] bounds = typeVariable.getBounds();
		if (bounds.length == 0) {
			return Object.class;
		}
		Type bound = bounds[0];
		if (bound instanceof TypeVariable) {
			bound = extractBoundForTypeVariable((TypeVariable) bound);
		}
		return bound;
	}
	
	private static void extractTypeVariablesFromGenericInterfaces(Type[] genericInterfaces, Map<TypeVariable, Type> typeVariableMap) {
		for (Type genericInterface : genericInterfaces) {
			if (genericInterface instanceof ParameterizedType) {
				ParameterizedType pt = (ParameterizedType) genericInterface;
				populateTypeMapFromParameterizedType(pt, typeVariableMap);
				if (pt.getRawType() instanceof Class) {
					extractTypeVariablesFromGenericInterfaces(
						((Class<?>) pt.getRawType()).getGenericInterfaces(), typeVariableMap);
				}
			} 
			else if (genericInterface instanceof Class) {
				extractTypeVariablesFromGenericInterfaces(
					((Class) genericInterface).getGenericInterfaces(), typeVariableMap);
			}
		}
	}
	
	private static void populateTypeMapFromParameterizedType(ParameterizedType type, Map<TypeVariable, Type> typeVariableMap) {
		if (type.getRawType() instanceof Class) {
			Type[] actualTypeArguments = type.getActualTypeArguments();
			TypeVariable[] typeVariables = ((Class<?>)type.getRawType()).getTypeParameters();
			for (int i = 0; i < actualTypeArguments.length; i++) {
				Type actualTypeArgument = actualTypeArguments[i];
				TypeVariable variable = typeVariables[i];
				if (actualTypeArgument instanceof Class) {
					typeVariableMap.put(variable, actualTypeArgument);
				}
				else if (actualTypeArgument instanceof GenericArrayType) {
					typeVariableMap.put(variable, actualTypeArgument);
				}
				else if (actualTypeArgument instanceof ParameterizedType) {
					typeVariableMap.put(variable, actualTypeArgument);
				}
				else if (actualTypeArgument instanceof TypeVariable) {
					TypeVariable typeVariableArgument = (TypeVariable) actualTypeArgument;
					Type resolvedType = typeVariableMap.get(typeVariableArgument);
					if (resolvedType == null) {
						resolvedType = extractBoundForTypeVariable(typeVariableArgument);
					}
					typeVariableMap.put(variable, resolvedType);
				}
			}
		}
	}
}
