package org.yuan.study.spring.beans.factory.config;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Type;

import org.yuan.study.spring.core.GenericCollectionTypeResolver;
import org.yuan.study.spring.core.MethodParameter;
import org.yuan.study.spring.core.ParameterNameDiscoverer;
import org.yuan.study.spring.util.Assert;

public class DependencyDescriptor implements Serializable {
	private static final long serialVersionUID = 1L;

	private transient MethodParameter methodParameter;
	
	private transient Field field;
	
	private Class<?> declaringClass;
	
	private String methodName;
	
	private Class[] parameterTypes;
	
	private int parameterIndex;
	
	private String fieldName;
	
	private final boolean required;
	
	private final boolean eager;
	
	private transient Annotation[] fieldAnnotations;

	/**
	 * Create a new descriptor for a method or constructor parameter.
	 * @param methodParameter
	 * @param required
	 * @param eager
	 */
	public DependencyDescriptor(MethodParameter methodParameter, boolean required, boolean eager) {
		Assert.notNull(methodParameter, "MethodParameter must not be null");
		
		this.methodParameter = methodParameter;
		this.declaringClass = methodParameter.getDeclaringClass();
		if (this.methodParameter.getMethod() != null) {
			this.methodName = methodParameter.getMethod().getName();
			this.parameterTypes = methodParameter.getMethod().getParameterTypes();
		} 
		else {
			this.parameterTypes = methodParameter.getConstructor().getParameterTypes();
		}
		this.parameterIndex = methodParameter.getParameterIndex();
		this.required = required;
		this.eager = eager;
	}

	/**
	 * Create a new descriptor for a method or constructor parameter.
	 * @param methodParameter
	 * @param required
	 */
	public DependencyDescriptor(MethodParameter methodParameter, boolean required) {
		this(methodParameter, required, true);
	}

	/**
	 * Create a new descriptor for a field.
	 * @param field
	 * @param required
	 */
	public DependencyDescriptor(Field field, boolean required) {
		this(field, required, true);
	}

	/**
	 * Create a new descriptor for a field.
	 * @param field
	 * @param required
	 * @param eager
	 */
	public DependencyDescriptor(Field field, boolean required, boolean eager) {
		Assert.notNull(field, "Field must not be null");
		
		this.field = field;
		this.declaringClass = field.getDeclaringClass();
		this.fieldName = field.getName();
		this.required = required;
		this.eager = eager;
	}

	/**
	 * Return the wrapped MethodParameter, if any.
	 * @return
	 */
	public MethodParameter getMethodParameter() {
		return methodParameter;
	}

	/**
	 * Return the wrapped Field, if any.
	 * @return
	 */
	public Field getField() {
		return field;
	}

	/**
	 * Return whether this dependency is required.
	 * @return
	 */
	public boolean isRequired() {
		return required;
	}

	/**
	 * Return whether this dependency is 'eager' in the sense of
	 * eagerly resolving potential target beans for type matching.
	 * @return
	 */
	public boolean isEager() {
		return eager;
	}
	
	/**
	 * Initialize parameter name discovery for the underlying method parameter, if any.
	 * @param parameterNameDiscoverer
	 */
	public void initParameterNameDiscovery(ParameterNameDiscoverer parameterNameDiscoverer) {
		if (methodParameter != null) {
			methodParameter.initParameterNameDiscovery(parameterNameDiscoverer);
		}
	}
	
	/**
	 * Determine the name of the wrapped parameter/field.
	 * @return
	 */
	public String getDependencyName() {
		return (field != null ? field.getName() : methodParameter.getParameterName());
	}
	
	/**
	 * Determine the name of the wrapped parameter/field.
	 * @return
	 */
	public Class<?> getDependencyType() {
		return (field != null ? field.getType() : methodParameter.getParameterType());
	}
	
	/**
	 * Determine the declared type of the wrapped parameter/field.
	 * @return
	 */
	public Type getGenericDependencyType() {
		return (field != null ? field.getGenericType() : methodParameter.getGenericParameterType());
	}
	
	/**
	 * Determine the generic type of the wrapped parameter/field.
	 * @return
	 */
	public Class<?> getCollectionType() {
		return field != null 
			? GenericCollectionTypeResolver.getCollectionFieldType(field)
			: GenericCollectionTypeResolver.getCollectionParameterType(methodParameter);
	}
	
	/**
	 * Determine the generic key type of the wrapped Collection parameter/field, if any.
	 * @return
	 */
	public Class<?> getMapKeyType() {
		return field != null 
			? GenericCollectionTypeResolver.getMapKeyFieldType(field)
			: GenericCollectionTypeResolver.getMapKeyParameterType(methodParameter);
	}
	
	/**
	 * Determine the geneic value type of the wrapped Map parameter/field, if any.
	 * @return
	 */
	public Class<?> getMapValueType() {
		return field != null 
			? GenericCollectionTypeResolver.getMapValueFieldType(field)
			: GenericCollectionTypeResolver.getMapValueParameterType(methodParameter);
	}
	
	/**
	 * Obtain the annotations associated with the wrapped parameter/field, if any.
	 * @return
	 */
	public Annotation[] getAnnotations() {
		if (field != null) {
			if (fieldAnnotations == null) {
				fieldAnnotations = field.getAnnotations();
			}
			return fieldAnnotations;
		}
		else {
			return methodParameter.getParameterAnnotations();
		}
	}
	
	private void readObject(ObjectInputStream ois) throws IOException, ClassNotFoundException {
		ois.defaultReadObject();
		
		try {
			if (fieldName != null) {
				field = declaringClass.getDeclaredField(fieldName);
			}
			else if (methodName != null) {
				methodParameter = new MethodParameter(declaringClass.getDeclaredMethod(methodName, parameterTypes), parameterIndex);
			}
			else {
				methodParameter = new MethodParameter(declaringClass.getDeclaredConstructor(parameterTypes), parameterIndex);
			}
		} 
		catch (Throwable ex) {
			throw new IllegalStateException("Could not find original class structure", ex);
		}
	}
}
