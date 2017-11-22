package org.yuan.study.spring.core;

import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.HashMap;
import java.util.Map;

import org.yuan.study.spring.util.Assert;

public class MethodParameter {

	private Method method;
	
	private Constructor<?> constructor;
	
	private final int parameterIndex;
	
	private Class<?> parameterType;
	
	private Type genericParameterType;
	
	private Annotation[] parameterAnnotations;
	
	private ParameterNameDiscoverer parameterNameDiscoverer;
	
	private String parameterName;
	
	private int nestingLevel = 1;
	
	private Map<Integer, Integer> typeIndexesPerLevel;
	
	Map<TypeVariable<?>, Type> typeVariableMap;

	
	/**
	 * Create a new MethodParameter for the given method, with nesting level 1.
	 * @param method
	 * @param parameterIndex
	 */
	public MethodParameter(Method method, int parameterIndex) {
		this(method, parameterIndex, 1);
	}

	/**
	 * Create a new MethodParameter for the given method.
	 * @param method
	 * @param parameterIndex
	 * @param nestingLevel
	 */
	public MethodParameter(Method method, int parameterIndex, int nestingLevel) {
		Assert.notNull(method, "Method must not be null");
		
		this.method = method;
		this.parameterIndex = parameterIndex;
		this.nestingLevel = nestingLevel;
	}

	/**
	 * Create a new MethodParameter for the given constructor, with nesting level 1.
	 * @param constructor
	 * @param parameterIndex
	 * @param nestingLevel
	 */
	public MethodParameter(Constructor<?> constructor, int parameterIndex) {
		this(constructor, parameterIndex, 1);
	}
	
	/**
	 * Create a new MethodParameter for the given constructor.
	 * @param constructor
	 * @param parameterIndex
	 * @param nestingLevel
	 */
	public MethodParameter(Constructor<?> constructor, int parameterIndex, int nestingLevel) {
		Assert.notNull(constructor, "Constructor must not be null");
		
		this.constructor = constructor;
		this.parameterIndex = parameterIndex;
		this.nestingLevel = nestingLevel;
	}

	/**
	 * Copy constructor, resulting in an independent MethodParameter object 
	 * based on the same metadata and cache state that the original object was in.
	 * @param original
	 */
	public MethodParameter(MethodParameter original) {
		Assert.notNull(original, "Original must not be null");
		
		this.method = original.method;
		this.constructor = original.constructor;
		this.parameterIndex = original.parameterIndex;
		this.parameterType = original.parameterType;
		this.parameterAnnotations = original.parameterAnnotations;
		this.typeVariableMap = original.typeVariableMap;
	}

	/**
	 * Return the type of the method/constructor parameter.
	 * @return
	 */
	public Class<?> getParameterType() {
		if (parameterType == null) {
			if (parameterIndex < 0) {
				parameterType = (method != null ? method.getReturnType() : null);
			} 
			else {
				parameterType = (method != null ? 
					method.getParameterTypes()[parameterIndex] : 
					constructor.getParameterTypes()[parameterIndex]);
			}
		}
		
		return parameterType;
	}

	/**
	 * Set a resolved generic parameter type.
	 * @param parameterType
	 */
	public void setParameterType(Class<?> parameterType) {
		this.parameterType = parameterType;
	}

	/**
	 * Return the wrapped Method, if any.
	 * @return
	 */
	public Method getMethod() {
		return method;
	}

	/**
	 * Return the index of the method/constructor parameter.
	 * @return
	 */
	public int getParameterIndex() {
		return parameterIndex;
	}

	/**
	 * Return the generic type of the method/constructor parameter.
	 * @return
	 */
	public Type getGenericParameterType() {
		if (genericParameterType == null) {
			if (parameterIndex < 0) {
				genericParameterType = (method != null ? method.getGenericReturnType() : null);
			} 
			else {
				genericParameterType = (method != null ? 
					method.getGenericParameterTypes()[parameterIndex] : 
					constructor.getGenericParameterTypes()[parameterIndex]);
			}
		}
		return genericParameterType;
	}

	/**
	 * Return the annotations associated with the specific method/constructor parameter.
	 * @return
	 */
	public Annotation[] getParameterAnnotations() {
		if (parameterAnnotations == null) {
			Annotation[][] annotationArray = (method != null ? 
				method.getParameterAnnotations() : constructor.getParameterAnnotations());
			if (parameterIndex >= 0 && parameterIndex < annotationArray.length) {
				parameterAnnotations = annotationArray[parameterIndex];
			} 
			else {
				parameterAnnotations = new Annotation[0];
			}
		}
		
		return parameterAnnotations;
	}

	/**
	 * Return the nesting level of the target type.
	 * @return
	 */
	public int getNestingLevel() {
		return nestingLevel;
	}

	/**
	 * Return the class that declares the underlying Method or Constructor.
	 * @return
	 */
	public Class<?> getDeclaringClass() {
		return (this.method != null ? this.method.getDeclaringClass() : this.constructor.getDeclaringClass());
	}
	
	/**
	 * Return the annotations associated with the target method/constructor itself.
	 * @return
	 */
	public Annotation[] getMethodAnnotations() {
		return (method != null ? method.getAnnotations() : constructor.getAnnotations());
	}
	
	/**
	 * Return the method/constructor annotation of the given type, if available.
	 * @param annotationType
	 * @return
	 */
	public <T extends Annotation> T getMethodAnnotation(Class<T> annotationType) {
		return (T)(method != null ? method.getAnnotation(annotationType) : constructor.getAnnotation(annotationType));
	}
	
	/**
	 * Return the parameter annotation of the given type, if available.
	 * @param annotationType
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public <T extends Annotation> T getParameterAnnotation(Class<T> annotationType) {
		Annotation[] annos = getParameterAnnotations();
		for (Annotation anno : annos) {
			if (annotationType.isInstance(anno)) {
				return (T) anno;
			}
		}
		return null;
	}
	
	/**
	 * Initialize parameter name discovery for this method parameter.
	 * @param parameterNameDiscoverer
	 */
	public void initParameterNameDiscovery(ParameterNameDiscoverer parameterNameDiscoverer) {
		this.parameterNameDiscoverer = parameterNameDiscoverer;
	}

	/**
	 * Return the name of the method/constructor parameter.
	 * @return
	 */
	public String getParameterName() {
		if (parameterNameDiscoverer != null) {
			String[] parameterNames = (method != null ? 
				parameterNameDiscoverer.getParameterNames(method) : 
				parameterNameDiscoverer.getParameterNames(constructor));
			if (parameterNames != null) {
				parameterName = parameterNames[parameterIndex];
			}
			parameterNameDiscoverer = null;
		}
		
		return parameterName;
	}
	
	/**
	 * Increase this parameter's nesting level.
	 */
	public void increaseNestingLevel() {
		nestingLevel++;
	}
	
	/**
	 * Decrease this parameter's nesting level.
	 */
	public void decreaseNestingLevel() {
		getTypeIndexesPerLevel().remove(nestingLevel);
		nestingLevel--;
	}
	
	/**
	 * Set the type index for the current nesting level.
	 * @param typeIndex
	 */
	public void setTypeIndexForCurrentLevel(int typeIndex) {
		getTypeIndexesPerLevel().put(nestingLevel, typeIndex);
	}
	
	/**
	 * Return the type index for the current nesting level.
	 * @return
	 */
	public Integer getTypeIndexForCurrentLevel() {
		return getTypeIndexForLevel(nestingLevel);
	}
	
	/**
	 * Return the type index for the specified nesting level.
	 * @param nestingLevel
	 * @return
	 */
	public Integer getTypeIndexForLevel(int nestingLevel) {
		return getTypeIndexesPerLevel().get(nestingLevel);
	}
	
	/**
	 * Obtain the type-indexes-per-level Map.
	 * @return
	 */
	private Map<Integer, Integer> getTypeIndexesPerLevel() {
		if (typeIndexesPerLevel == null) {
			typeIndexesPerLevel = new HashMap<Integer, Integer>(4);
		}
		
		return typeIndexesPerLevel;
	}
	
	/**
	 * Create a new MethodParameter for the given method or constructor.
	 * @param methodOrConstructor
	 * @param parameterIndex
	 * @return
	 */
	public static MethodParameter forMethodOrConstructor(Object methodOrConstructor, int parameterIndex) {
		if (methodOrConstructor instanceof Method) {
			return new MethodParameter((Method) methodOrConstructor, parameterIndex);
		} 
		else if (methodOrConstructor instanceof Constructor) {
			return new MethodParameter((Constructor<?>) methodOrConstructor, parameterIndex);
		}
		else {
			throw new IllegalArgumentException(String.format("Given object [%s] is neither a Method nor a Constructor", methodOrConstructor));
		}
	}

	/**
	 * Return the wrapped Constructor, if any.
	 * @param constructor
	 */
	public Constructor<?> getConstructor() {
		return constructor;
	}
	
}
