package org.yuan.study.spring.beans;

import java.beans.IntrospectionException;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;

import org.yuan.study.spring.core.BridgeMethodResolver;
import org.yuan.study.spring.core.MethodParameter;
import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.StringUtils;

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
		
		Method readMethodToUse = BridgeMethodResolver.findBridgeMethod(readMethod);
		Method writeMethodToUse = BridgeMethodResolver.findBridgeMethod(writeMethod);
		if (writeMethodToUse == null && readMethodToUse != null) {
			writeMethodToUse = ClassUtils.getMethodIfAvailable(this.beanClass, 
				"set" + StringUtils.capitalize(getName()), readMethodToUse.getReturnType());
		}
		this.readMethod = readMethodToUse;
		this.writeMethod = writeMethodToUse;
		
		if (this.writeMethod != null && this.readMethod == null) {
			Set<Method> ambiguousCandidates = new HashSet<Method>();
			for (Method method : beanClass.getMethods()) {
				if (method.getName().equals(writeMethodToUse.getName()) 
					&& !method.equals(writeMethodToUse) && !method.isBridge()) {
					ambiguousCandidates.add(method);
				}
			}
			if (!ambiguousCandidates.isEmpty()) {
				this.ambiguousWriteMethods = ambiguousCandidates;
			}
		}
	}

	@Override
	public Method getReadMethod() {
		return readMethod;
	}

	@Override
	public Method getWriteMethod() {
		return writeMethod;
	}

	@Override
	public synchronized Class<?> getPropertyType() {
		return propertyEditorClass;
	}

	@Override
	public Class<?> getPropertyEditorClass() {
		return propertyEditorClass;
	}
	
	public synchronized MethodParameter getWriteMethodParameter() {
		if (writeMethod == null) {
			return null;
		}
		if (writeMethodParameter == null) {
			writeMethodParameter = new MethodParameter(writeMethod, 0);
		}
		return writeMethodParameter;
	}

}
