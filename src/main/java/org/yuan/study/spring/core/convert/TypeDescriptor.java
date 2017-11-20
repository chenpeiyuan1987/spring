package org.yuan.study.spring.core.convert;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.yuan.study.spring.core.MethodParameter;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ClassUtils;

public class TypeDescriptor {
	
	public static final TypeDescriptor NULL = new TypeDescriptor();
	
	private static final TypeDescriptor UNKNOWN = new TypeDescriptor(Object.class);
	
	private static final Map<Class<?>, TypeDescriptor> typeDescriptorCache = new HashMap<Class<?>, TypeDescriptor>();

	private static final Annotation[] EMPTY_ANNOTATION_ARRAY = new Annotation[0];
	
	static {
		typeDescriptorCache.put(boolean.class, new TypeDescriptor(boolean.class));
		typeDescriptorCache.put(Boolean.class, new TypeDescriptor(Boolean.class));
		typeDescriptorCache.put(byte.class, new TypeDescriptor(byte.class));
		typeDescriptorCache.put(Byte.class, new TypeDescriptor(Byte.class));
		typeDescriptorCache.put(char.class, new TypeDescriptor(char.class));
		typeDescriptorCache.put(Character.class, new TypeDescriptor(Character.class));
		typeDescriptorCache.put(short.class, new TypeDescriptor(short.class));
		typeDescriptorCache.put(Short.class, new TypeDescriptor(Short.class));
		typeDescriptorCache.put(int.class, new TypeDescriptor(int.class));
		typeDescriptorCache.put(Integer.class, new TypeDescriptor(Integer.class));
		typeDescriptorCache.put(long.class, new TypeDescriptor(long.class));
		typeDescriptorCache.put(Long.class, new TypeDescriptor(Long.class));
		typeDescriptorCache.put(float.class, new TypeDescriptor(float.class));
		typeDescriptorCache.put(Float.class, new TypeDescriptor(Float.class));
		typeDescriptorCache.put(double.class, new TypeDescriptor(double.class));
		typeDescriptorCache.put(Double.class, new TypeDescriptor(Double.class));
		typeDescriptorCache.put(String.class, new TypeDescriptor(String.class));
	}
	
	private Class<?> type;
	
	private MethodParameter methodParameter;
	
	private Field field;
	
	private int fieldNestingLevel = 1;
	
	private Object value;
	
	private volatile TypeDescriptor elementType;
	
	private volatile TypeDescriptor mapKeyType;
	
	private volatile TypeDescriptor mapValueType;
	
	private volatile Annotation[] annotations;
	
	
	/**
	 * Create a new type descriptor from a method or constructor parameter.
	 * @param methodParameter
	 */
	public TypeDescriptor(MethodParameter methodParameter) {
		Assert.notNull(methodParameter, "MethodParameter must not be null");
		this.methodParameter = methodParameter;
	}
	
	/**
	 * Create a new type descriptor for a field.
	 * @param methodParameter
	 */
	public TypeDescriptor(Field field) {
		Assert.notNull(field, "Field must not be null");
		this.field = field;
	}
	
	/**
	 * Create a new type descriptor from a method or constructor parameter.
	 * @param methodParameter
	 */
	public TypeDescriptor(MethodParameter methodParameter, Class<?> type) {
		Assert.notNull(methodParameter, "MethodParameter must not be null");
		this.type = type;
		this.methodParameter = methodParameter;
	}
	
	/**
	 * Create a new type descriptor for a field.
	 * @param field
	 * @param type
	 */
	public TypeDescriptor(Field field, Class<?> type) {
		Assert.notNull(field, "Field must not be null");
		this.type = type;
		this.field = field;
	}
	
	/**
	 * Create a new type descriptor for a field.
	 * @param field
	 * @param nestingLevel
	 * @param type
	 */
	public TypeDescriptor(Field field, int nestingLevel, Class<?> type) {
		Assert.notNull(field, "Field must not be null");
		this.type = type;
		this.field = field;
		this.fieldNestingLevel = nestingLevel;
	}
	
	/**
	 * Internal constructor for a NULL descriptor.
	 */
	public TypeDescriptor() {
		
	}
	
	/**
	 * Create a new descriptor for the type of the given value.
	 * @param value
	 */
	public TypeDescriptor(Object value) {
		Assert.notNull(value, "Value must not be null");
		this.type = value.getClass();
		this.value = value;
	}
	
	/**
	 * Create a new descriptor for the given type.
	 * @param type
	 */
	public TypeDescriptor(Class<?> type) {
		Assert.notNull(type, "Type must not be null");
		this.type = type;
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
	 * 
	 * @return
	 */
	public Class<?> getType() {
		if (type != null) {
			return type;
		}
		if (field != null) {
			return field.getType();
		}
		if (methodParameter != null) {
			return methodParameter.getParameterType();
		}
		return null;
	}
	
	public Class<?> getObjectType() {
		Class<?> type = getType();
		if (type == null) {
			return null;
		}
		
		return ClassUtils.resolvePrimitiveIfNecessary(type);
	}
	
	public String getName() {
		Class<?> type = getType();
		if (type == null) {
			return null;
		}
		
		return ClassUtils.getQualifiedName(type);
	}
	
	public boolean isPrimitive() {
		Class<?> type = getType();
		if (type == null) {
			return false;
		}
		if (!type.isPrimitive()) {
			return false;
		}
		
		return true;
	}
	
	public boolean isArray() {
		Class<?> type = getType();
		if (type == null) {
			return false;
		}
		if (!type.isArray()) {
			return false;
		}
		
		return true;
	}
	
	public boolean isCollection() {
		return Collection.class.isAssignableFrom(getType());
	}
	
	public Class<?> getElementType() {
		return getElementTypeDescriptor().getType();
	}
	
	public TypeDescriptor getElementTypeDescriptor() {
		if (elementType == null) {
			elementType = forElementType(resolveElementType());
		}
		return elementType;
	}
	
	public TypeDescriptor getElementTypeDescriptor(Object element) {
		TypeDescriptor elementType = getElementTypeDescriptor();
		if (!TypeDescriptor.UNKNOWN.equals(elementType)) {
			return elementType;
		}
		return forObject(element);
	}
	
	public boolean isMap() {
		return Map.class.isAssignableFrom(getType());
	}
	
	public boolean isMapEntryTypeKnown() {
		
	}
	
	//--------------------------------------------------
	// internal helpers
	//--------------------------------------------------
	
	private Class<?> resolveElementType() {
		if (isArray()) {
			
		}
	}
	
	//--------------------------------------------------
	// static factory methods
	//--------------------------------------------------
	
	public static TypeDescriptor forObject(Object object) {
		if (object == null) {
			return TypeDescriptor.NULL;
		}
		if (object instanceof Collection<?> || object instanceof Map<?, ?>) {
			return new TypeDescriptor(object);
		}
		
		return valueOf(object.getClass());
	}
	
	public static TypeDescriptor valueOf(Class<?> type) {
		if (type == null) {
			return TypeDescriptor.NULL;
		}
		TypeDescriptor desc = typeDescriptorCache.get(type);
		return (desc != null ? desc : new TypeDescriptor(type));
	}
}
