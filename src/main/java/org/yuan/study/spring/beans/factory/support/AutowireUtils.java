package org.yuan.study.spring.beans.factory.support;

import java.beans.PropertyDescriptor;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Set;

import org.springframework.beans.BeanUtils;
import org.springframework.util.ClassUtils;

public abstract class AutowireUtils {
	
	/**
	 * Sort the given constructors, preferring public constructors and "greedy" ones 
	 * with a maximum of arguments. The result will contain public constructors first,
	 * with decreasing number of arguments, then non-public constructors, again with 
	 * decreasing number of arguments.
	 * @param constructors
	 */
	public static void sortConstructors(Constructor<?>[] constructors) {
		Arrays.sort(constructors, new Comparator<Object>() {
			@Override
			public int compare(Object o1, Object o2) {
				Constructor<?> c1 = (Constructor<?>) o1;
				Constructor<?> c2 = (Constructor<?>) o2;
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
	 * Determine a weight that represents the class hierarchy difference between types and 
	 * arguments.
	 * @param argTypes
	 * @param args
	 * @return
	 */
	public static int getTypeDifferenceWeight(Class<?>[] argTypes, Object[] args) {
		int result = 0;
		for (int i = 0; i < argTypes.length; i++) {
			if (!BeanUtils.isAssignable(argTypes[i], args[i])) {
				return Integer.MAX_VALUE;
			}
			if (args[i] != null) {
				Class<?> superClass = args[i].getClass().getSuperclass();
				while (superClass != null) {
					if (BeanUtils.isAssignable(argTypes[i], superClass)) {
						result++;
						superClass = superClass.getSuperclass();
					}
					else {
						superClass = null;
					}
				}
			}
		}
		return result;
	}
	
	/**
	 * Determine whether the given bean property is excluded from dependency checks.
	 * @param propertyDescriptor
	 * @return
	 */
	public static boolean isExcludedFromDependencyCheck(PropertyDescriptor propertyDescriptor) {
		Method writeMethod = propertyDescriptor.getWriteMethod();
		if (writeMethod.getDeclaringClass().getName().indexOf("$$") == -1) {
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
}
