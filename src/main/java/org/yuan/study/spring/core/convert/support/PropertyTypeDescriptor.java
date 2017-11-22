package org.yuan.study.spring.core.convert.support;

import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.Map;

import org.yuan.study.spring.core.MethodParameter;
import org.yuan.study.spring.core.convert.TypeDescriptor;
import org.yuan.study.spring.util.ReflectionUtils;
import org.yuan.study.spring.util.StringUtils;

public class PropertyTypeDescriptor extends TypeDescriptor {

	private final PropertyDescriptor propertyDescriptor;
	
	private Annotation[] cachedAnnotations;
	
	/**
	 * Create a new BeanTypeDescriptor for the given bean property.
	 * @param propertyDescriptor
	 * @param methodParameter
	 */
	public PropertyTypeDescriptor(PropertyDescriptor propertyDescriptor, MethodParameter methodParameter) {
		super(methodParameter);
		this.propertyDescriptor = propertyDescriptor;
	}
	
	/**
	 * Create a new BeanTypeDescriptor for the given bean property.
	 * @param propertyDescriptor
	 * @param methodParameter
	 * @param type
	 */
	public PropertyTypeDescriptor(PropertyDescriptor propertyDescriptor, MethodParameter methodParameter, Class<?> type) {
		super(methodParameter, type);
		this.propertyDescriptor = propertyDescriptor;
	}
	
	/**
	 * Return the underlying PropertyDescriptor.
	 * @return
	 */
	public PropertyDescriptor getPropertyDescriptor() {
		return propertyDescriptor;
	}
	
	public Annotation[] getAnnotations() {
		Annotation[] anns = cachedAnnotations;
		if (anns == null) {
			Map<Class<?>, Annotation> annMap = new LinkedHashMap<Class<?>, Annotation>();
			String name = propertyDescriptor.getName();
			if (StringUtils.hasLength(name)) {
				Class<?> clazz = getMethodParameter().getMethod().getDeclaringClass();
				Field field = ReflectionUtils.findField(clazz, name);
				if (field == null) {
					field = ReflectionUtils.findField(clazz, name.substring(0, 1).toLowerCase() + name.substring(1));
					if (field == null) {
						field = ReflectionUtils.findField(clazz, name.substring(0, 1).toUpperCase() + name.substring(1));
					}
				}
				if (field != null) {
					for (Annotation ann : field.getAnnotations()) {
						annMap.put(ann.annotationType(), ann);
					}
				}
			}
			
			Method writeMethod = propertyDescriptor.getWriteMethod();
			if (writeMethod != null && writeMethod != getMethodParameter().getMethod()) {
				for (Annotation ann : writeMethod.getAnnotations()) {
					annMap.put(ann.annotationType(), ann);
				}
			}
			Method readMethod = propertyDescriptor.getReadMethod();
			if (readMethod != null && readMethod != getMethodParameter().getMethod()) {
				for (Annotation ann : readMethod.getAnnotations()) {
					annMap.put(ann.annotationType(), ann);
				}
			}
			for (Annotation ann : getMethodParameter().getMethodAnnotations()) {
				annMap.put(ann.annotationType(), ann);
			}
			for (Annotation ann : getMethodParameter().getParameterAnnotations()) {
				annMap.put(ann.annotationType(), ann);
			}
			
			anns = annMap.values().toArray(new Annotation[annMap.size()]);
			cachedAnnotations = anns;
		}
		return anns;
	}
	
	@Override
	public TypeDescriptor forElementType(Class<?> elementType) {
		if (elementType != null) {
			MethodParameter nested = new MethodParameter(getMethodParameter());
			nested.increaseNestingLevel();
			return new PropertyTypeDescriptor(propertyDescriptor, nested, elementType);
		} 
		else {
			return super.forElementType(null);
		}
	}
}
