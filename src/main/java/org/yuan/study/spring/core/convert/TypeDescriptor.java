package org.yuan.study.spring.core.convert;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.yuan.study.spring.core.GenericCollectionTypeResolver;
import org.yuan.study.spring.core.MethodParameter;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.CollectionUtils;
import org.yuan.study.spring.util.ObjectUtils;

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
		if (!isMap()) {
			return false;
		}
		if (getMapKeyType() == null) {
			return false;
		}
		if (getMapValueType() == null) {
			return false;
		}
		return true;
	}
	
	public Class<?> getMapKeyType() {
		return getMapKeyTypeDescriptor().getType();
	}
	
	public TypeDescriptor getMapKeyTypeDescriptor() {
		if (mapKeyType == null) {
			mapKeyType = forElementType(resolveMapKeyType());
		}
		return mapKeyType;
	}
	
	public TypeDescriptor getMapKeyTypeDescriptor(Object key) {
		TypeDescriptor keyType = getMapKeyTypeDescriptor();
		if (TypeDescriptor.UNKNOWN.equals(keyType)) {
			return TypeDescriptor.forObject(key);
		}
		return keyType;
	}
	
	public Class<?> getMapValueType() {
		return getMapValueTypeDescriptor().getType();
	}
	
	public TypeDescriptor getMapValueTypeDescriptor() {
		if (mapValueType == null) {
			mapValueType = forElementType(resolveMapValueType());
		}
		return mapValueType;
	}
	
	public TypeDescriptor getMapValueTypeDescriptor(Object value) {
		TypeDescriptor valueType = getMapValueTypeDescriptor();
		if (TypeDescriptor.UNKNOWN.equals(valueType)) {
			return TypeDescriptor.forObject(value);
		}
		return valueType;
	}
	
	public Annotation[] getAnnotations() {
		if (annotations == null) {
			annotations = resolveAnnotations();
		}
		return annotations;
	}
	
	public Annotation getAnnotation(Class<? extends Annotation> annotationType) {
		for (Annotation annotation : getAnnotations()) {
			if (annotation.annotationType().equals(annotationType)) {
				return annotation;
			}
		}
		return null;
	}
	
	public boolean isAssignableTo(TypeDescriptor targetType) {
		if (this == TypeDescriptor.NULL || targetType == TypeDescriptor.NULL) {
			return true;
		}
		if (isCollection() && targetType.isCollection() || isArray() && targetType.isArray()) {
			return targetType.getType().isAssignableFrom(getType())
				&& getElementTypeDescriptor().isAssignableTo(targetType.getElementTypeDescriptor());
		}
		if (isMap() && targetType.isMap()) {
			return targetType.getType().isAssignableFrom(getType())
				&& getMapKeyTypeDescriptor().isAssignableTo(targetType.getMapKeyTypeDescriptor())
				&& getMapValueTypeDescriptor().isAssignableTo(targetType.getMapValueTypeDescriptor());
		}
		return targetType.getObjectType().isAssignableFrom(getObjectType());
	}
	
	public TypeDescriptor forElementType(Class<?> elementType) {
		if (elementType == null) {
			return TypeDescriptor.UNKNOWN;
		}
		if (methodParameter != null) {
			MethodParameter nested = new MethodParameter(methodParameter);
			nested.increaseNestingLevel();
			return new TypeDescriptor(nested, elementType);
		}
		if (field != null) {
			return new TypeDescriptor(field, fieldNestingLevel + 1, elementType);
		}
		return TypeDescriptor.valueOf(elementType);
	}
	
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof TypeDescriptor)) {
			return false;
		}
		TypeDescriptor other = (TypeDescriptor) obj;
		boolean annotatedTypeEquals = getType().equals(other.getType()) 
			&& ObjectUtils.nullSafeEquals(getAnnotations(), other.getAnnotations());
		if (isCollection()) {
			return annotatedTypeEquals && ObjectUtils.nullSafeEquals(getElementType(), other.getElementType());
		}
		if (isMap()) {
			return annotatedTypeEquals && ObjectUtils.nullSafeEquals(getMapKeyType(), other.getMapKeyType())
				&& ObjectUtils.nullSafeEquals(getMapValueType(), other.getMapValueType());
		}
		return annotatedTypeEquals;
	}
	
	public int hashCode() {
		if (this == TypeDescriptor.NULL) {
			return 0;
		}
		return getType().hashCode();
	}
	
	public String asString() {
		return toString();
	}
	
	public String toString() {
		if (this == TypeDescriptor.NULL) {
			return "null";
		}
		else {
			StringBuilder builder = new StringBuilder();
			Annotation[] anns = getAnnotations();
			for (Annotation ann : anns) {
				builder.append("@").append(ann.annotationType().getName()).append(" ");
			}
			builder.append(ClassUtils.getQualifiedName(getType()));
			if (isMap()) {
				builder.append("<").append(getMapKeyTypeDescriptor());
				builder.append(", ").append(getMapValueTypeDescriptor()).append(">");
			}
			else if (isCollection()) {
				builder.append("<").append(getElementTypeDescriptor()).append(">");
			}
			return builder.toString();
		}
	}
	
	//--------------------------------------------------
	// internal helpers
	//--------------------------------------------------
	
	private Class<?> resolveElementType() {
		if (isArray()) {
			return getType().getComponentType();
		}
		if (isCollection()) {
			return resolveCollectionElementType();
		}
		return null;
	}
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	private Class<?> resolveCollectionElementType() {
		if (field != null) {
			return GenericCollectionTypeResolver.getCollectionFieldType(field, fieldNestingLevel);
		}
		if (methodParameter != null) {
			return GenericCollectionTypeResolver.getCollectionParameterType(methodParameter);
		}
		if (value instanceof Collection) {
			Class<?> type = CollectionUtils.findCommonElementType((Collection) value);
			if (type != null) {
				return type;
			}
		}
		if (type != null) {
			return GenericCollectionTypeResolver.getCollectionType((Class<? extends Collection>) type);
		}
		return null;
	}
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	private Class<?> resolveMapKeyType() {
		if (field != null) {
			return GenericCollectionTypeResolver.getMapKeyFieldType(field);
		}
		if (methodParameter != null) {
			return GenericCollectionTypeResolver.getMapKeyParameterType(methodParameter);
		}
		if (value instanceof Map<?, ?>) {
			Class<?> keyType = CollectionUtils.findCommonElementType(((Map<?, ?>) value).keySet());
			if (keyType != null) {
				return keyType;
			}
		}
		if (type != null && isMap()) {
			return GenericCollectionTypeResolver.getMapKeyType((Class<? extends Map>) type);
		}
		return null;
	}
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	private Class<?> resolveMapValueType() {
		if (field != null) {
			return GenericCollectionTypeResolver.getMapValueFieldType(field);
		}
		if (methodParameter != null) {
			return GenericCollectionTypeResolver.getMapValueParameterType(methodParameter);
		}
		if (value instanceof Map<?, ?>) {
			Class<?> type = CollectionUtils.findCommonElementType(((Map<?, ?>) value).values());
			if (type != null) {
				return type;
			}
		}
		if (type != null && isMap()) {
			return GenericCollectionTypeResolver.getMapValueType((Class<? extends Map>) type);
		}
		return null;
	}
	
	private Annotation[] resolveAnnotations() {
		if (field != null) {
			return field.getAnnotations();
		}
		if (methodParameter != null) {
			if (methodParameter.getParameterIndex() < 0) {
				return methodParameter.getMethodAnnotations();
			}
			return methodParameter.getParameterAnnotations();
		}
		return EMPTY_ANNOTATION_ARRAY;
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
