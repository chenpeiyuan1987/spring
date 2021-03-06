package org.yuan.study.spring.beans;

import java.beans.IntrospectionException;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.yuan.study.spring.core.BridgeMethodResolver;
import org.yuan.study.spring.core.GenericTypeResolver;
import org.yuan.study.spring.core.MethodParameter;
import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.StringUtils;

public class GenericTypeAwarePropertyDescriptor extends PropertyDescriptor {
	
	private static final Log LOG = LogFactory.getLog(GenericTypeAwarePropertyDescriptor.class);
	
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
		if (propertyType == null) {
			if (readMethod != null) {
				propertyType = GenericTypeResolver.resolveReturnType(readMethod, beanClass);
			}
			else {
				MethodParameter writeMethodParam = getWriteMethodParameter();
				if (writeMethodParam != null) {
					propertyType = writeMethodParam.getParameterType();
				}
				else {
					propertyType = super.getPropertyType();
				}
			}
		}
		
		return propertyType;
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
			GenericTypeResolver.resolveParameterType(writeMethodParameter, beanClass);
		}
		return writeMethodParameter;
	}

	public Method getWriteMethodForActualAccess() {
		if (ambiguousWriteMethods != null) {
			ambiguousWriteMethods = null;
			LOG.warn(String.format(
				"Invalid JavaBean property '%s' being accessed! Ambiguous write methods found next to actually used [%s]: %s", 
					getName(), writeMethod, ambiguousWriteMethods));
		}
		return writeMethod;
	}
}
