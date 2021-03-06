package org.yuan.study.spring.core;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.Collection;
import java.util.Map;

@SuppressWarnings("rawtypes")
public abstract class GenericCollectionTypeResolver {

	/**
	 * Determine the generic element type of the given Collection class.
	 * @param collectionClass
	 * @return
	 */
	public static Class<?> getCollectionType(Class<? extends Collection> collectionClass) {
		return extractTypeFromClass(collectionClass, Collection.class, 0);
	}
	
	/**
	 * Determine the generic key type of the given Map class.
	 * @param mapClass
	 * @return
	 */
	public static Class<?> getMapKeyType(Class<? extends Map> mapClass) {
		return extractTypeFromClass(mapClass, Map.class, 0);
	}
	
	/**
	 * Determine the generic value type of the given Map class.
	 * @param mapClass
	 * @return
	 */
	public static Class<?> getMapValueType(Class<? extends Map> mapClass) {
		return extractTypeFromClass(mapClass, Map.class, 1);
	}
	
	/**
	 * Determine the generic element type of the given Collection field.
	 * @param collectionField
	 * @return
	 */
	public static Class<?> getCollectionFieldType(Field collectionField) {
		return getGenericFieldType(collectionField, Collection.class, 0, 1);
	}
	
	/**
	 * Determine the generic element type of the given Collection field.
	 * @param collectionField
	 * @return
	 */
	public static Class<?> getCollectionFieldType(Field collectionField, int nestingLevel) {
		return getGenericFieldType(collectionField, Collection.class, 0, nestingLevel);
	}
	
	/**
	 * Determine the generic key type of the given Map field.
	 * @param mapField
	 * @return
	 */
	public static Class<?> getMapKeyFieldType(Field mapField) {
		return getGenericFieldType(mapField, Map.class, 0, 1);
	}
	
	/**
	 * Determine the generic key type of the given Map field.
	 * @param mapField
	 * @return
	 */
	public static Class<?> getMapKeyFieldType(Field mapField, int nestingLevel) {
		return getGenericFieldType(mapField, Map.class, 0, nestingLevel);
	}
	
	/**
	 * Determine the generic value type of the given Map field.
	 * @param mapField
	 * @return
	 */
	public static Class<?> getMapValueFieldType(Field mapField) {
		return getGenericFieldType(mapField, Map.class, 1, 1);
	}
	
	/**
	 * Determine the generic value type of the given Map field.
	 * @param mapField
	 * @return
	 */
	public static Class<?> getMapValueFieldType(Field mapField, int nestingLevel) {
		return getGenericFieldType(mapField, Map.class, 1, nestingLevel);
	}
	
	/**
	 * Determine the generic element type of the given Collection parameter.
	 * @param collectionClass
	 * @return
	 */
	public static Class<?> getCollectionParameterType(MethodParameter methodParam) {
		return getGenericParameterType(methodParam, Collection.class, 0);
	}
	
	/**
	 * Determine the generic key type of the given Map parameter.
	 * @param mapClass
	 * @return
	 */
	public static Class<?> getMapKeyParameterType(MethodParameter methodParam) {
		return getGenericParameterType(methodParam, Map.class, 0);
	}
	
	/**
	 * Determine the generic value type of the given Map parameter.
	 * @param mapClass
	 * @return
	 */
	public static Class<?> getMapValueParameterType(MethodParameter methodParam) {
		return getGenericParameterType(methodParam, Map.class, 1);
	}
	
	/**
	 * Determine the generic element type of the given Collection return type.
	 * @param collectionClass
	 * @return
	 */
	public static Class<?> getCollectionReturnType(Method method) {
		return getGenericReturnType(method, Collection.class, 0, 1);
	}
	
	/**
	 * Determine the generic element type of the given Collection return type.
	 * @param collectionClass
	 * @return
	 */
	public static Class<?> getCollectionReturnType(Method method, int nestingLevel) {
		return getGenericReturnType(method, Collection.class, 0, nestingLevel);
	}
	
	/**
	 * Determine the generic key type of the given Map parameter.
	 * @param mapClass
	 * @return
	 */
	public static Class<?> getMapKeyReturnType(Method method) {
		return getGenericReturnType(method, Map.class, 0, 1);
	}
	
	/**
	 * Determine the generic key type of the given Map parameter.
	 * @param mapClass
	 * @return
	 */
	public static Class<?> getMapKeyReturnType(Method method, int nestingLevel) {
		return getGenericReturnType(method, Map.class, 0, nestingLevel);
	}
	
	/**
	 * Determine the generic value type of the given Map parameter.
	 * @param mapClass
	 * @return
	 */
	public static Class<?> getMapValueReturnType(Method method) {
		return getGenericReturnType(method, Map.class, 1, 1);
	}
	
	/**
	 * Determine the generic value type of the given Map parameter.
	 * @param mapClass
	 * @return
	 */
	public static Class<?> getMapValueReturnType(Method method, int nestingLevel) {
		return getGenericReturnType(method, Map.class, 1, nestingLevel);
	}
	
	/**
	 * Extract the generic parameter type from the given method or constructor.
	 * @param methodParam
	 * @param source
	 * @param typeIndex
	 * @return
	 */
	private static Class<?> getGenericParameterType(MethodParameter methodParam, Class<?> source, int typeIndex) {
		return extractType(methodParam, GenericTypeResolver.getTargetType(methodParam), source, typeIndex, methodParam.getNestingLevel(), 1);
	}
	
	/**
	 * Extract the generic type from the given field.
	 * @param field
	 * @param source
	 * @param typeIndex
	 * @param nestingLevel
	 * @return
	 */
	private static Class<?> getGenericFieldType(Field field, Class<?> source, int typeIndex, int nestingLevel) {
		return extractType(null, field.getGenericType(), source, typeIndex, nestingLevel, 1);
	}
	
	/**
	 * Extract the generic return type from the given method.
	 * @param method
	 * @param source
	 * @param typeIndex
	 * @param nestingLevel
	 * @return
	 */
	private static Class<?> getGenericReturnType(Method method, Class<?> source, int typeIndex, int nestingLevel) {
		return extractType(null, method.getGenericReturnType(), source, typeIndex, nestingLevel, 1);
	}
	
	/**
	 * Extract the generic return type from the given Type object.
	 * @param methodParam
	 * @param type
	 * @param source
	 * @param typeIndex
	 * @param nestingLevel
	 * @param currentLevel
	 * @return
	 */
	private static Class<?> extractType(MethodParameter methodParam, Type type, Class<?> source, int typeIndex, int nestingLevel, int currentLevel) {
		Type resolvedType = type;
		if (type instanceof TypeVariable && methodParam != null && methodParam.typeVariableMap != null) {
			Type mappedType = methodParam.typeVariableMap.get((TypeVariable)type);
			if (mappedType != null) {
				resolvedType = mappedType;
			}
		}
		if (resolvedType instanceof ParameterizedType) {
			return extractTypeFromParameterizedType(methodParam, (ParameterizedType)resolvedType, source, typeIndex, nestingLevel, currentLevel);
		}
		if (resolvedType instanceof Class) {
			return extractTypeFromClass(methodParam, (Class<?>)resolvedType, source, typeIndex, nestingLevel, currentLevel);
		}
		return null;
	}
	
	
	/**
	 * Extract the generic type from the given ParameterizedType object.
	 * @param methodParam
	 * @param ptype
	 * @param source
	 * @param typeIndex
	 * @param nestingLevel
	 * @param currentLevel
	 * @return
	 */
	private static Class<?> extractTypeFromParameterizedType(MethodParameter methodParam, ParameterizedType ptype, Class<?> source, int typeIndex, int nestingLevel, int currentLevel) {
		if (!(ptype.getRawType() instanceof Class)) {
			return null;
		}
		
		Class<?> rawType = (Class<?>) ptype.getRawType();
		Type[] paramTypes = ptype.getActualTypeArguments();
		if (nestingLevel - currentLevel > 0) {
			int nextLevel = currentLevel + 1;
			Integer currentTypeIndex = (methodParam != null ? methodParam.getTypeIndexForLevel(nextLevel) : null);
			int indexToUse = (currentTypeIndex != null ? currentTypeIndex : paramTypes.length - 1);
			Type paramType = paramTypes[indexToUse];
			return extractType(methodParam, paramType, source, typeIndex, nestingLevel, nextLevel);
		}
		if (source != null && !source.isAssignableFrom(rawType)) {
			return null;
		}
		Class<?> fromSuperclassOrInterface = extractTypeFromClass(methodParam, rawType, source, typeIndex, nestingLevel, currentLevel);
		if (fromSuperclassOrInterface != null) {
			return fromSuperclassOrInterface;
		}
		if (paramTypes == null || typeIndex >= paramTypes.length) {
			return null;
		}
		Type paramType = paramTypes[typeIndex];
		if (paramType instanceof TypeVariable && methodParam != null && methodParam.typeVariableMap != null) {
			Type mappedType = methodParam.typeVariableMap.get((TypeVariable) paramType);
			if (mappedType != null) {
				paramType = mappedType;
			}
		}
		if (paramType instanceof WildcardType) {
			WildcardType wildcardType = (WildcardType)paramType;
			Type[] upperBounds = wildcardType.getUpperBounds();
			if (upperBounds != null && upperBounds.length > 0 && !Object.class.equals(upperBounds[0])) {
				paramType = upperBounds[0];
			}
			else {
				Type[] lowerBounds = wildcardType.getLowerBounds();
				if (lowerBounds != null && lowerBounds.length > 0 && !Object.class.equals(lowerBounds[0])) {
					paramType = lowerBounds[0];
				}
			}
		}
		if (paramType instanceof ParameterizedType) {
			paramType = ((ParameterizedType)paramType).getRawType();
		}
		if (paramType instanceof GenericArrayType) {
			Type compType = ((GenericArrayType)paramType).getGenericComponentType();
			if (compType instanceof Class) {
				return Array.newInstance((Class<?>)compType, 0).getClass();
			}
		}
		else if (paramType instanceof Class) {
			return (Class<?>) paramType;
		}
		
		return null;
	}
	
	
	/**
	 * Extract the generic type from the given Class object.
	 * @param methodParam
	 * @param clazz
	 * @param source
	 * @param typeIndex
	 * @param nestingLevel
	 * @param currentLevel
	 * @return
	 */
	private static Class<?> extractTypeFromClass(Class<?> clazz, Class<?> source, int typeIndex) {
		return extractTypeFromClass(null, clazz, source, typeIndex, 1, 1);
	}
	
	/**
	 * Extract the generic type from the given Class object.
	 * @param methodParam
	 * @param clazz
	 * @param source
	 * @param typeIndex
	 * @param nestingLevel
	 * @param currentLevel
	 * @return
	 */
	private static Class<?> extractTypeFromClass(MethodParameter methodParam, Class<?> clazz, Class<?> source, int typeIndex, int nestingLevel, int currentLevel) {
		if (clazz.getName().startsWith("java.util.")) {
			return null;
		}
		
		if (clazz.getSuperclass() != null && isIntrospectionCandidate(clazz.getSuperclass())) {
			return extractType(methodParam, clazz.getGenericSuperclass(), source, typeIndex, nestingLevel, currentLevel);
		}
		
		Type[] ifcs = clazz.getGenericInterfaces();
		if (ifcs != null) {
			for (Type ifc : ifcs) {
				Type rawType = ifc;
				if (ifc instanceof ParameterizedType) {
					rawType = ((ParameterizedType)ifc).getRawType();
				}
				if (rawType instanceof Class && isIntrospectionCandidate((Class<?>)rawType)) {
					return extractType(methodParam, ifc, source, typeIndex, nestingLevel, currentLevel);
				}
			}
		}
		
		return null;
	}
	
	/**
	 * Determine whether the given class is a potential candidate 
	 * that defines generic collection or map types.
	 * @param clazz
	 * @return
	 */
	private static boolean isIntrospectionCandidate(Class<?> clazz) {
		return (Collection.class.isAssignableFrom(clazz) || Map.class.isAssignableFrom(clazz));
	}
}
