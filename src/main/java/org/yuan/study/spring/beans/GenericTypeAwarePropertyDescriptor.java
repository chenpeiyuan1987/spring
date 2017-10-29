package org.yuan.study.spring.beans;

import java.beans.IntrospectionException;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Method;
import java.util.Set;

import org.yuan.study.spring.core.MethodParameter;

public class GenericTypeAwarePropertyDescriptor extends PropertyDescriptor {
	
	private final Class<?> beanClass;
	
	private final Method readMethod;
	
	private final Method writeMethod;

	private final Class<?> propertyEditorClass;
	
	private volatile Set<Method> ambiguousWriteMethods;
	
	private Class<?> propertyType;
	
	private MethodParameter writeMethodParameter;

	public GenericTypeAwarePropertyDescriptor(Class<?> beanClass, String propertyName,
		Method readMethod, Method writeMethod, Class<?> propertyEditorClass)
		throws IntrospectionException {
		
		super(propertyName, null, null);
		this.beanClass = beanClass;
		this.propertyEditorClass = propertyEditorClass;
		
		
		this.readMethod = readMethod;
		this.writeMethod = writeMethod;
	}

	@Override
	public Method getReadMethod() {
		return readMethod;
	}

	@Override
	public Method getWriteMethod() {
		return writeMethod;
	}
	
	

}
