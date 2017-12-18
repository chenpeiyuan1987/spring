package org.yuan.study.spring.beans.factory.support;

import java.beans.PropertyDescriptor;
import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Set;

import org.yuan.study.spring.beans.factory.ObjectFactory;
import org.yuan.study.spring.util.ClassUtils;

public abstract class AutowireUtils {
	
	/**
	 * Sort the given constructors, preferring public constructors and "greedy" ones 
	 * with a maximum of arguments. The result will contain public constructors first,
	 * with decreasing number of arguments, then non-public constructors, again with 
	 * decreasing number of arguments.
	 * @param constructors
	 */
	public static void sortConstructors(Constructor<?>[] constructors) {
		Arrays.sort(constructors, new Comparator<Constructor<?>>() {
			@Override
			public int compare(Constructor<?> c1, Constructor<?> c2) {
				boolean p1 = Modifier.isPublic(c1.getModifiers());
				boolean p2 = Modifier.isPublic(c2.getModifiers());
				if (p1 != p2) {
					return (p1 ? -1 : 1);
				}
				int c1p1 = c1.getParameterTypes().length;
				int c2p1 = c2.getParameterTypes().length;
				return (new Integer(c1p1)).compareTo(new Integer(c2p1)) * -1;
			}
		});
	}
	
	/**
	 * Sort the given factory methods, preferring public methods and "greedy" ones 
	 * with a maximum of arguments. The result will contain public methods first,
	 * with decreasing number of arguments, then non-public methods, again with 
	 * decreasing number of arguments.
	 * @param constructors
	 */
	public static void sortFactoryMethods(Method[] methods) {
		Arrays.sort(methods, new Comparator<Method>() {
			@Override
			public int compare(Method m1, Method m2) {
				boolean p1 = Modifier.isPublic(m1.getModifiers());
				boolean p2 = Modifier.isPublic(m2.getModifiers());
				if (p1 != p2) {
					return (p1 ? -1 : 1);
				}
				int c1p1 = m1.getParameterTypes().length;
				int c2p1 = m2.getParameterTypes().length;
				return (new Integer(c1p1)).compareTo(new Integer(c2p1)) * -1;
			}
		});
	}
	
	/**
	 * Determine whether the given bean property is excluded from dependency checks.
	 * @param propertyDescriptor
	 * @return
	 */
	public static boolean isExcludedFromDependencyCheck(PropertyDescriptor propertyDescriptor) {
		Method writeMethod = propertyDescriptor.getWriteMethod();
		if (writeMethod == null) {
			return false;
		}
		if (!writeMethod.getDeclaringClass().getName().contains("$$")) {
			return false;
		}
		
		Class<?> superClass = writeMethod.getDeclaringClass().getSuperclass();
		return !ClassUtils.hasMethod(superClass, writeMethod.getName(), writeMethod.getParameterTypes());
	}
	
	/**
	 * Return whether the setter method of the given bean property is defined
	 * in any of the given interfaces.
	 * @param propertyDescriptor
	 * @param interfaces
	 * @return
	 */
	public static boolean isSetterDefinedInInterface(PropertyDescriptor propertyDescriptor, Set<Class<?>> interfaces) {
		Method setter = propertyDescriptor.getWriteMethod();
		if (setter != null) {
			Class<?> targetClass = setter.getDeclaringClass();
			for (Class<?> superClass : interfaces) {
				if (superClass.isAssignableFrom(targetClass) 
					&& ClassUtils.hasMethod(superClass, setter.getName(), setter.getParameterTypes())) {
					return true;
				}
			}
		}
		
		return false;
	}
	
	/**
	 * Resolve the given autowiring value against the given required type,
	 * e.g. an ObjectFactory value to its actual object result.
	 * @return
	 */
	public static Object resolveAutowiringValue(Object autowiringValue, Class<?> requiredType) {
		if (autowiringValue instanceof ObjectFactory && !requiredType.isInstance(autowiringValue)) {
			ObjectFactory<?> factory = (ObjectFactory<?>) autowiringValue;
			if (autowiringValue instanceof Serializable && requiredType.isInterface()) {
				autowiringValue = Proxy.newProxyInstance(requiredType.getClassLoader(), 
					new Class[]{requiredType}, new ObjectFactoryDelegatingInvocationHandler(factory));
			} 
			else {
				return factory.getObject();
			}
		}
		
		return autowiringValue;
	}
	
	/**
	 * Reflective InvocationHandler for lazy access to the current target object.
	 */
	private static class ObjectFactoryDelegatingInvocationHandler implements InvocationHandler, Serializable {
		private static final long serialVersionUID = 1L;
		
		private final ObjectFactory<?> objectFactory;
		
		public ObjectFactoryDelegatingInvocationHandler(ObjectFactory<?> objectFactory) {
			this.objectFactory = objectFactory;
		}

		@Override
		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			String methodName = method.getName();
			if (methodName.equals("equals")) {
				return (proxy == args[0]);
			} 
			else if (methodName.equals("hashCode")) {
				return System.identityHashCode(proxy);
			}
			else if (methodName.equals("toString")) {
				return this.objectFactory.toString();
			}
			try {
				return method.invoke(objectFactory.getObject(), args);
			} 
			catch (InvocationTargetException ex) {
				throw ex.getTargetException();
			}
		}
	}
}
