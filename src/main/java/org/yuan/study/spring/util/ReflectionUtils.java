package org.yuan.study.spring.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

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
	 * Set the field represented by the supplied field on the specified target to 
	 * the specified value.
	 * @param field
	 * @param target
	 * @param value
	 */
	public static void setField(Field field, Object target, Object value) {
		try {
			field.set(target, value);
		} 
		catch (IllegalAccessException ex) {
			handleReflectionException(ex);
			throw new IllegalStateException(String.format("Unexpected reflection expection - %s: %s", ex.getClass().getName(), ex.getMessage()));
		}
	}
	
	/**
	 * Get the field represented by the supplied field on the specified target.
	 * In accordance with Field.get(Object) semantics, the returned value is automatically
	 * wrapped if the underlying field has a primitive type.
	 * @param field
	 * @param target
	 * @return
	 */
	public static Object getField(Field field, Object target) {
		try {
			return field.get(target);
		} 
		catch (IllegalAccessException ex) {
			handleReflectionException(ex);
			throw new IllegalStateException(String.format("Unexpected reflection expection - %s: %s", ex.getClass().getName(), ex.getMessage()));
		}
	}
	
	/**
	 * Attempt to find a Method on the supplied class with the supplied name
	 * and no parameters. Searches all superclasses up to Object.
	 * Return null if no Method can be found.
	 * @param clazz
	 * @param name
	 * @return
	 */
	public static Method findMethod(Class<?> clazz, String name) {
		return findMethod(clazz, name, new Class[0]);
	}
	
	/**
	 * Attempt to find a Method on the supplied class with the supplied name
	 * and no parameters. Searches all superclasses up to Object.
	 * Return null if no Method can be found.
	 * @param targetClass
	 * @param methodName
	 * @param paramTypes
	 * @return
	 */
	public static Method findMethod(Class<?> clazz, String name, Class<?>... paramTypes) {
		Assert.notNull(clazz, "Class must not be null");
		Assert.notNull(name, "Method name must not be null");
		
		Class<?> searchType = clazz;
		while (searchType != null) {
			Method[] methods = (searchType.isInterface() ? searchType.getMethods() : searchType.getDeclaredMethods());
			for (Method method : methods) {
				if (name.equals(method.getName()) && 
					(paramTypes == null || Arrays.equals(paramTypes, method.getParameterTypes()))) {
					return method;
				}
			}
			searchType = searchType.getSuperclass();
		}
		return null;
	}
	
	/**
	 * Invoke the specified Method against the supplied target object with no arguments.
	 * The target object can be null when invoking a static Method.
	 * Thrown exceptions are handled via a call to handleReflectionException.
	 * @param method
	 * @param target
	 * @return
	 */
	public static Object invokeMethod(Method method, Object target) {
		return invokeMethod(method, target, new Object[0]);
	}
	
	/**
	 * Invoke the specified Method against the supplied target object with the supplied arguments.
	 * The target object can be null when invoking a static Method.
	 * Thrown exceptions are handled via a call to handleReflectionException.
	 * @param method
	 * @param target
	 * @param args
	 * @return
	 */
	public static Object invokeMethod(Method method, Object target, Object... args) {
		try {
			return method.invoke(target, args);
		} 
		catch (Exception ex) {
			handleReflectionException(ex);
		}
		throw new IllegalStateException("Should never get here");
	}
	
	/**
	 * Invoke the specified JDBC API Method against the supplied target 
	 * object with no arguments.
	 * @param method
	 * @param target
	 * @return
	 * @throws SQLException
	 */
	public static Object invokeJdbcMethod(Method method, Object target) throws SQLException {
		return invokeJdbcMethod(method, target, new Object[0]);
	}
	
	/**
	 * Invoke the specified JDBC API Method against the supplied target 
	 * object with the supplied arguments.
	 * @param method
	 * @param target
	 * @param args
	 * @return
	 * @throws SQLException
	 */
	public static Object invokeJdbcMethod(Method method, Object target, Object... args) throws SQLException {
		try {
			return method.invoke(target, args);
		} 
		catch (IllegalAccessException ex) {
			handleReflectionException(ex);
		}
		catch (InvocationTargetException ex) {
			if (ex.getTargetException() instanceof SQLException) {
				throw (SQLException) ex.getTargetException();
			}
			handleInvocationTargetException(ex);
		}
		throw new IllegalStateException("Should never get here");
	}
	
	/**
	 * Handle the given reflection exception. Should only be called if no
	 * checked exception is expected to be thrown by the target method.
	 * @param ex
	 */
	public static void handleReflectionException(Exception ex) {
		if (ex instanceof NoSuchMethodException) {
			throw new IllegalStateException("Method not found: " + ex.getMessage());
		}
		if (ex instanceof IllegalAccessException) {
			throw new IllegalStateException("Could not access method: " + ex.getMessage());
		}
		if (ex instanceof InvocationTargetException) {
			handleInvocationTargetException((InvocationTargetException) ex);
		}
		if (ex instanceof RuntimeException) {
			throw (RuntimeException) ex;
		}
		handleUnexpectedException(ex);;
	}
	
	/**
	 * Handle the given reflection exception. Should only be called if no
	 * checked exception is expected to be thrown by the target method.
	 * @param ex
	 */
	public static void handleInvocationTargetException(InvocationTargetException ex) {
		rethrowRuntimeException(ex.getTargetException());
	}
	
	/**
	 * Rethrow the given exception, which is presumably the target exception of an
	 * InvocationTargetException. Should only be called if no checked exception is
	 * expected to be thrown by the target method.
	 */
	public static void rethrowRuntimeException(Throwable ex) {
		if (ex instanceof RuntimeException) {
			throw (RuntimeException) ex;
		}
		if (ex instanceof Error) {
			throw (Error) ex;
		}
		handleUnexpectedException(ex);
	}
	
	/**
	 * Rethrow the given exception, which is presumably the target exception of an
	 * InvocationTargetException. Should only be called if no checked exception is
	 * expected to be thrown by the target method.
	 */
	public static void rethrowException(Throwable ex) throws Exception {
		if (ex instanceof Exception) {
			throw (Exception) ex;
		}
		if (ex instanceof Error) {
			throw (Error) ex;
		}
		handleUnexpectedException(ex);
	}
	
	/**
	 * Throws an IllegalStateException with the given exception as root cause.
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
	 * Perform the given callback operation on all matching methods of the given class
	 * and superclasses.
	 * @param clazz
	 * @param mc
	 */
	public static void doWithMethods(Class<?> clazz, MethodCallback mc) throws IllegalArgumentException {
		doWithMethods(clazz, mc, null);
	}
	
	/**
	 * Perform the given callback operation on all matching methods of the given class
	 * and superclasses.
	 * @param clazz
	 * @param mc
	 * @param mf
	 * @throws IllegalArgumentException
	 */
	public static void doWithMethods(Class<?> clazz, MethodCallback mc, MethodFilter mf) throws IllegalArgumentException {
		Method[] methods = clazz.getDeclaredMethods();
		for (Method method : methods) {
			if (mf != null && !mf.matches(method)) {
				continue;
			}
			try {
				mc.doWith(method);
			} 
			catch (IllegalAccessException ex) {
				throw new IllegalStateException(String.format(
					"Shouldn't be illegal to access method '%s': %s", method.getName(), ex));
			}
		}
		if (clazz.getSuperclass() != null) {
			doWithMethods(clazz.getSuperclass(), mc, mf);
		}
		else if (clazz.isInterface()) {
			for (Class<?> clazz : clazz.getInterfaces()) {
				doWithMethods(clazz, mc, mf);
			}
		}
	}
	
	/**
	 * Get all declared methods on the leaf class and all superclasses.
	 * Leaf class methods are included first.
	 * @param clazz
	 * @return
	 * @throws IllegalArgumentException
	 */
	public static Method[] getAllDeclaredMethods(Class<?> clazz) throws IllegalArgumentException {
		final List<Method> methods = new ArrayList<Method>(32);
		doWithMethods(clazz, new MethodCallback() {
			@Override
			public void doWith(Method method) {
				methods.add(method);
			}
		});
		return methods.toArray(new Method[methods.size()]);
	}
	
	/**
	 * Invoke the given callback on all fields in the target class, going up the class
	 * hierarchy to get all declared fields.
	 * @param clazz
	 * @param fc
	 * @throws IllegalArgumentException
	 */
	public static void doWithFields(Class<?> clazz, FieldCallback fc) throws IllegalArgumentException {
		doWithFields(clazz, fc, null);
	}
	
	/**
	 * Invoke the given callback on all fields in the target class, going up the class
	 * hierarchy to get all declared fields.
	 * @param clazz
	 * @param fc
	 * @param ff
	 * @throws IllegalArgumentException
	 */
	public static void doWithFields(Class<?> clazz, FieldCallback fc, FieldFilter ff) throws IllegalArgumentException {
		Class<?> target = clazz;
		do {
			Field[] fields = target.getDeclaredFields();
			for (Field field : fields) {
				if (ff != null && !ff.matches(field)) {
					continue;
				}
				try {
					fc.doWith(field);
				}
				catch(IllegalAccessException ex) {
					throw new IllegalStateException(String.format("Shouldn't be illegal to access field '%s': %s", field.getName(), ex));
				}
				
			}
			target = clazz.getSuperclass();
		}
		while(target != null && target != Object.class);
	}
	
	/**
	 * Given the source object and the destination, which must be the same class or a subclass,
	 * copy all fields, including inherited fields. Designed to work on  objects with public no-arg constructors.
	 * @param src
	 * @param dst
	 * @throws IllegalArgumentException
	 */
	public static void shallowCopyFieldState(final Object src, final Object dst) throws IllegalArgumentException {
		Assert.notNull(src, "Source for field copy cannot be null");
		Assert.notNull(dst, "Destination for field copy cannot be null");
		if (!src.getClass().isAssignableFrom(dst.getClass())) {
			throw new IllegalArgumentException(String.format(
				"Destination class [%s] must be same or subclass as source class [%s]", 
					dst.getClass().getName(), src.getClass().getName()));
		}
		
		doWithFields(src.getClass(), new FieldCallback() {
			@Override
			public void doWith(Field field) throws IllegalArgumentException, IllegalAccessException {
				makeAccessible(field);
				Object val = field.get(src);
				field.set(dst, val);
			}
		}, COPYABLE_FIELDS);
	}
	
	//----------------------------------------------------------------------------
	//
	//----------------------------------------------------------------------------
	
	/**
	 * Action to take on each method.
	 */
	public interface MethodCallback {
		
		/**
		 * Perform an operation using the given method.
		 * @param method
		 * @throws IllegalArgumentException
		 * @throws IllegalAccessException
		 */
		void doWith(Method method) throws IllegalArgumentException, IllegalAccessException;
	}
	
	/**
	 * Callback optionally used to method fields to be operated on by a method callback.
	 */
	public interface MethodFilter {
		
		/**
		 * Determine whether the given method matches.
		 * @param method
		 * @return
		 */
		boolean matches(Method method);
	}
	
	/**
	 * Callback interface invoked on each field in the hierarchy.
	 */
	public interface FieldCallback {
		
		/**
		 * Perform an operation using the given field.
		 * @param field
		 * @throws IllegalArgumentException
		 * @throws IllegalArgumentException
		 */
		void doWith(Field field) throws IllegalArgumentException, IllegalAccessException;
	}
	
	/**
	 * Callback optionally used to filter fields to be operated on by a field callback.
	 */
	public interface FieldFilter {
		
		/**
		 * Determine whether the given field matches.
		 * @param field
		 * @return
		 */
		boolean matches(Field field);
	}
	
	/**
	 * Pre-built FieldFilter that matches all non-static, non-final fields.
	 */
	public static FieldFilter COPYABLE_FIELDS = new FieldFilter() {
		@Override
		public boolean matches(Field field) {
			return !(Modifier.isStatic(field.getModifiers()) || Modifier.isFinal(field.getModifiers()));
		}
	};
	
	/**
	 * Pre-built MethodFilter that matches all non-bridge methods.
	 */
	public static MethodFilter NON_BRIDGED_METHODS = new MethodFilter() {
		@Override
		public boolean matches(Method method) {
			return !method.isBridge();
		}
	};
	
	/**
	 * Pre-built MethodFilter that matches all non-bridge methods
	 * which are not declared on java.lang.Object.
	 */
	public static MethodFilter USER_DECLARED_METHODS = new MethodFilter() {
		@Override
		public boolean matches(Method method) {
			return (!method.isBridge() && method.getDeclaringClass() != Object.class);
		}
	};
}
