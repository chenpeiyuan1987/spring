package org.yuan.study.spring.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import net.sf.cglib.transform.MethodFilter;

public abstract class ReflectionUtils {
	
	/**
	 * Attempt to find a Field on the supplied Class with the supplied name.
	 * Searches all superclasses up to Object. 
	 * @param clazz
	 * @param name
	 * @return
	 */
	public static Field findField(Class<?> clazz, String name) {
		return findField(clazz, name, null);
	}
	
	/**
	 * Attempt to find a Field on the supplied Class with the supplied name and/or type.
	 * Searches all superclasses up to Object.
	 * @param clazz
	 * @param name
	 * @param type
	 * @return
	 */
	public static Field findField(Class<?> clazz, String name, Class<?> type) {
		Assert.notNull(clazz, "Class must no be null");
		Assert.isTrue(name != null || type != null, "Either name or type of the field must be specified");
		
		Class<?> searchType = clazz;
		while (!Object.class.equals(searchType) && searchType != null) {
			Field[] fields = searchType.getDeclaredFields();
			for (Field field : fields) {
				if ((name == null || name.equals(field.getName())) && (type == null || type.equals(field.getType()))) {
					return field;
				}
			}
			searchType = clazz.getSuperclass();
		}
		
		return null;
	}
	
	/**
	 * 
	 * @param field
	 * @param target
	 * @param value
	 */
	public static void setField(Field field, Object target, Object value) {
		
	}
	
	/**
	 * 
	 * @param field
	 * @param target
	 * @return
	 */
	public static Object getField(Field field, Object target) {
		
	}
	
	/**
	 * 
	 * @param clazz
	 * @param name
	 * @return
	 */
	public static Method findMethod(Class<?> clazz, String name) {
		return findMethod(clazz, name, new Class[0]);
	}
	
	/**
	 * 
	 * @param targetClass
	 * @param methodName
	 * @param paramTypes
	 * @return
	 */
	public static Method findMethod(Class<?> targetClass, String methodName, Class<?>... paramTypes) {
		Assert.notNull(targetClass, "Class must not be null");
		Assert.notNull(methodName, "Method name must not be null");
		
	}
	
	/**
	 * 
	 * @param method
	 * @param target
	 * @return
	 */
	public static Object invokeMethod(Method method, Object target) {
		return invokeMethod(method, target, new Object[0]);
	}
	
	/**
	 * 
	 * @param method
	 * @param target
	 * @param args
	 * @return
	 */
	public static Object invokeMethod(Method method, Object target, Object... args) {
		
	}
	
	/**
	 * 
	 * @param ex
	 */
	public static void handleReflectionException(Exception ex) {
		
	}
	
	/**
	 * 
	 */
	public static void handleInvocationTargetException(InvocationTargetException ex) {
		
	}
	
	/**
	 * 
	 */
	public static void rethrowRuntimeException(Throwable ex) {
		
	}
	
	/**
	 * 
	 */
	public static void rethrowException(Throwable ex) {
		
	}
	
	/**
	 * 
	 * @param ex
	 */
	public static void handleUnexpectedException(Throwable ex) {
		throw new IllegalStateException("Unexpected exception thrown", ex);
	}
	
	/**
	 * 
	 * @param method
	 * @param exceptionType
	 */
	public static void declaresException(Method method, Class<?> exceptionType) {
		
	}
	
	/**
	 * 
	 * @param field
	 * @return
	 */
	public static boolean isPublicStaticFinal(Field field) {
		
	}
	
	/**
	 * 
	 * @param method
	 * @return
	 */
	public static boolean isEqualsMethod(Method method) {
		
	}
	
	/**
	 * 
	 * @param method
	 * @return
	 */
	public static boolean isHashCodeMethod(Method method) {
		
	}
	
	/**
	 * 
	 * @return
	 */
	public static boolean isToStringMethod() {
		
	}
	
	/**
	 * 
	 * @param field
	 */
	public static void makeAccessible(Field field) {
		
	}
	
	/**
	 * 
	 * @param method
	 */
	public static void makeAccessible(Method method) {
		
	}
	
	/**
	 * 
	 * @param ctor
	 */
	public static void makeAccessible(Constructor<?> ctor) {
		
	}
	
	/**
	 * 
	 * @param clazz
	 * @param mc
	 */
	public static void doWithMethods(Class<?> clazz, MethodCallback mc) {
		
	}
	
	/**
	 * 
	 * @param clazz
	 * @param mc
	 * @param mf
	 * @throws IllegalArgumentException
	 */
	public static void doWithMethods(Class<?> clazz, MethodCallback mc, MethodFilter mf) throws IllegalArgumentException {
		
	}
	
	/**
	 * 
	 * @param clazz
	 * @return
	 * @throws IllegalArgumentException
	 */
	public static Method[] getAllDeclaredMethods(Class<?> clazz) throws IllegalArgumentException {
		
	}
	
	/**
	 * 
	 * @param clazz
	 * @param fc
	 * @throws IllegalArgumentException
	 */
	public static void doWithFields(Class<?> clazz, FieldCallback fc) throws IllegalArgumentException {
		
	}
	
	/**
	 * 
	 * @param clazz
	 * @param fc
	 * @param ff
	 * @throws IllegalArgumentException
	 */
	public static void doWithFields(Class<?> clazz, FieldCallback fc, FieldFilter ff) throws IllegalArgumentException {
		
	}
	
	/**
	 * 
	 * @param src
	 * @param dst
	 * @throws IllegalArgumentException
	 */
	public static void shallowCopyFieldState(final Object src, final Object dst) throws IllegalArgumentException {
		
	}
	
	//----------------------------------------------------------------------------
	//
	//----------------------------------------------------------------------------
	
	/**
	 * 
	 * @author Yuan
	 *
	 */
	public interface MethodCallback {
		
		/**
		 * 
		 * @throws IllegalArgumentException
		 * @throws IllegalAccessException
		 */
		void doWith() throws IllegalArgumentException, IllegalAccessException;
	}
	
	/**
	 * 
	 * @author Yuan
	 *
	 */
	public interface MethodFilter {
		
		/**
		 * 
		 * @param method
		 * @return
		 */
		boolean matches(Method method);
	}
	
	/**
	 * 
	 * @author Yuan
	 *
	 */
	public interface FieldCallback {
		
		/**
		 * 
		 * @param field
		 * @throws IllegalArgumentException
		 * @throws IllegalArgumentException
		 */
		void doWith(Field field) throws IllegalArgumentException, IllegalArgumentException;
	}
	
	/**
	 * 
	 * @author Yuan
	 *
	 */
	public interface FieldFilter {
		
		/**
		 * 
		 * @param field
		 * @return
		 */
		boolean matches(Field field);
	}
	
	/**
	 * 
	 */
	public static FieldFilter COPYABLE_FIELDS = new FieldFilter() {
		
	};
	
	/**
	 * 
	 */
	public static MethodFilter NON_BRIDGED_METHODS = new MethodFilter() {
		
	};
	
	/**
	 * 
	 */
	public static MethodFilter USER_DECLARED_METHODS = new MethodFilter() {
		
	};
}
