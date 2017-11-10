package org.yuan.study.spring.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.junit.Test;

import test.beans.DerivedTestBean;
import test.beans.IOther;
import test.beans.ITestBean;
import test.beans.TestBean;

public class ClassUtilsTest {

	@Test
	public void testIsPresent() throws Exception {
		assertTrue(ClassUtils.isPresent("java.lang.String"));
		assertFalse(ClassUtils.isPresent("java.lang.MySpecialString"));
	}
	
	@Test
	public void testForName() throws ClassNotFoundException {
		assertEquals(String.class, ClassUtils.forName("java.lang.String"));
		assertEquals(String[].class, ClassUtils.forName("java.lang.String[]"));
		assertEquals(String[][].class, ClassUtils.forName("java.lang.String[][]"));
		assertEquals(String[][][].class, ClassUtils.forName("java.lang.String[][][]"));
		assertEquals(String[].class, ClassUtils.forName(String[].class.getName()));
		assertEquals(String[][].class, ClassUtils.forName(String[][].class.getName()));
		assertEquals(String[][][].class, ClassUtils.forName(String[][][].class.getName()));
		assertEquals(int.class, ClassUtils.forName("int"));
		assertEquals(int[].class, ClassUtils.forName("int[]"));
		assertEquals(int[][].class, ClassUtils.forName("int[][]"));
		assertEquals(int[][][].class, ClassUtils.forName("int[][][]"));
		assertEquals(int[].class, ClassUtils.forName(int[].class.getName()));
		assertEquals(int[][].class, ClassUtils.forName(int[][].class.getName()));
		assertEquals(int[][][].class, ClassUtils.forName(int[][][].class.getName()));
	}
	
	@Test
	public void testForNameWithPrimitiveClasses() throws ClassNotFoundException {
		assertEquals(boolean.class, ClassUtils.forName("boolean"));
		assertEquals(char.class, ClassUtils.forName("char"));
		assertEquals(byte.class, ClassUtils.forName("byte"));
		assertEquals(short.class, ClassUtils.forName("short"));
		assertEquals(int.class, ClassUtils.forName("int"));
		assertEquals(long.class, ClassUtils.forName("long"));
		assertEquals(float.class, ClassUtils.forName("float"));
		assertEquals(double.class, ClassUtils.forName("double"));
		assertEquals(void.class, ClassUtils.forName("void"));
	}
	
	@Test
	public void testForNameWithPrimitiveArrays() throws ClassNotFoundException {
		assertEquals(boolean[].class, ClassUtils.forName("boolean[]"));
		assertEquals(char[].class, ClassUtils.forName("char[]"));
		assertEquals(byte[].class, ClassUtils.forName("byte[]"));
		assertEquals(short[].class, ClassUtils.forName("short[]"));
		assertEquals(int[].class, ClassUtils.forName("int[]"));
		assertEquals(long[].class, ClassUtils.forName("long[]"));
		assertEquals(float[].class, ClassUtils.forName("float[]"));
		assertEquals(double[].class, ClassUtils.forName("double[]"));
	}
	
	@Test
	public void testForNameWithPrimitiveArraysInternalName() throws ClassNotFoundException {
		assertEquals(boolean[].class, ClassUtils.forName(boolean[].class.getName()));
		assertEquals(char[].class, ClassUtils.forName(char[].class.getName()));
		assertEquals(byte[].class, ClassUtils.forName(byte[].class.getName()));
		assertEquals(short[].class, ClassUtils.forName(short[].class.getName()));
		assertEquals(int[].class, ClassUtils.forName(int[].class.getName()));
		assertEquals(long[].class, ClassUtils.forName(long[].class.getName()));
		assertEquals(float[].class, ClassUtils.forName(float[].class.getName()));
		assertEquals(double[].class, ClassUtils.forName(double[].class.getName()));
	}
	
	@Test
	public void testGetShortName() {
		String className = ClassUtils.getShortName(getClass());
		assertEquals("ClassUtilsTest", className);
	}
	
	@Test
	public void testGetShortNameForObjectArrayClass() {
		String className = ClassUtils.getShortName(Object[].class);
		assertEquals("Object[]", className);
	}
	
	@Test
	public void testGetShortNameForMultiDimensionalObjectArrayClass() {
		String className = ClassUtils.getShortName(Object[][].class);
		assertEquals("Object[][]", className);
	}
	@Test
	public void testGetShortNameForPrimitiveArrayClass() {
		String className = ClassUtils.getShortName(byte[].class);
		assertEquals("byte[]", className);
	}
	@Test
	public void testGetShortNameForMultiDimensionalPrimitiveArrayClass() {
		String className = ClassUtils.getShortName(byte[][][].class);
		assertEquals("byte[][][]", className);
	}
	
	@Test
	public void testGetShortNameForInnerClass() {
		String className = ClassUtils.getShortName(InnerClass.class);
		assertEquals("ClassUtilsTest.InnerClass", className);
	}
	
	@Test
	public void testGetShortNameAsProperty() {
		String className = ClassUtils.getShortNameAsProperty(this.getClass());
		assertEquals("classUtilsTest", className);
	}
	
	@Test
	public void testGetClassFileName() {
		assertEquals("String.class", ClassUtils.getClassFileName(String.class));
		assertEquals("ClassUtilsTest.class", ClassUtils.getClassFileName(getClass()));
	}
	
	@Test
	public void testGetPackageName() {
		assertEquals("java.lang", ClassUtils.getPackageName(String.class));
		assertEquals(getClass().getPackage().getName(), ClassUtils.getPackageName(getClass()));
	}
	
	@Test
	public void testGetQualifiedName() {
		String className = ClassUtils.getQualifiedName(getClass());
		assertEquals("org.yuan.study.spring.util.ClassUtilsTest", className);
	}
	
	@Test
	public void testGetQualifiedNameForObjectArrayClass() {
		String className = ClassUtils.getQualifiedName(Object[].class);
		assertEquals("java.lang.Object[]", className);
	}
	
	@Test
	public void testGetQualifiedNameForMultiDimensionalObjectArrayClass() {
		String className = ClassUtils.getQualifiedName(Object[][].class);
		assertEquals("java.lang.Object[][]", className);
	}
	
	@Test
	public void testGetQualifiedNameForPrimitiveArrayClass() {
		String className = ClassUtils.getQualifiedName(byte[].class);
		assertEquals("byte[]", className);
	}
	
	@Test
	public void testGetQualifiedNameForMultiDimensionalPrimitiveArrayClass() {
		String className = ClassUtils.getQualifiedName(byte[][].class);
		assertEquals("byte[][]", className);
	}
	
	@Test
	public void testHasMethod() {
		assertTrue(ClassUtils.hasMethod(Collection.class, "size"));
		assertTrue(ClassUtils.hasMethod(Collection.class, "remove", Object.class));
		assertFalse(ClassUtils.hasMethod(Collection.class, "remove"));
		assertFalse(ClassUtils.hasMethod(Collection.class, "someOtherMethod"));
	}
	
	@Test
	public void testGetMethodIfAvailable() {
		Method method = ClassUtils.getMethodIfAvailable(Collection.class, "size");
		assertNotNull(method);
		assertEquals("size", method.getName());
		
		method = ClassUtils.getMethodIfAvailable(Collection.class, "remove", Object.class);
		assertNotNull(method);
		assertEquals("remove", method.getName());
		
		assertNull(ClassUtils.getMethodIfAvailable(Collection.class, "remove"));
		assertNull(ClassUtils.getMethodIfAvailable(Collection.class, "someOtherMethod"));
	}
	
	@Test
	public void testGetMethodCountForName() {
		assertEquals(2, ClassUtils.getMethodCountForName(OverloadedMethodsClass.class, "print"));
		assertEquals(6, ClassUtils.getMethodCountForName(SubOverloadedMethodsClass.class, "print"));
	}
	
	@Test
	public void testCountOverloadedMethods() {
		assertFalse(ClassUtils.hasAtLeastOneMethodWithName(TestBean.class, "foobar"));
		
		assertTrue(ClassUtils.hasAtLeastOneMethodWithName(TestBean.class, "hashCode"));
		
		assertTrue(ClassUtils.hasAtLeastOneMethodWithName(TestBean.class, "setAge"));
	}
	
	@Test
	public void testNoArgsStaticMethod() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		Method method = ClassUtils.getStaticMethod(InnerClass.class, "staticMethod");
		method.invoke(null);
		assertTrue(InnerClass.noArgCalled);
	}
	
	@Test
	public void testArgsStaticMethod() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		Method method = ClassUtils.getStaticMethod(InnerClass.class, "argStaticMethod", String.class);
		method.invoke(null, "test");
		assertTrue(InnerClass.argCalled);
	}
	
	@Test
	public void testOverloadedStaticMethod() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		Method method = ClassUtils.getStaticMethod(InnerClass.class, "staticMethod", String.class);
		method.invoke(null, "test");
		assertTrue(InnerClass.overloadedCalled);
	}
	
	@Test
	public void testIsAssignable() {
		assertTrue(ClassUtils.isAssignable(Object.class, Object.class));
		assertTrue(ClassUtils.isAssignable(String.class, String.class));
		assertTrue(ClassUtils.isAssignable(Object.class, String.class));
		assertTrue(ClassUtils.isAssignable(Object.class, Integer.class));
		assertTrue(ClassUtils.isAssignable(Number.class, Integer.class));
		assertTrue(ClassUtils.isAssignable(Number.class, int.class));
		assertTrue(ClassUtils.isAssignable(Integer.class, int.class));
		assertTrue(ClassUtils.isAssignable(int.class, Integer.class));
		
		assertFalse(ClassUtils.isAssignable(String.class, Object.class));
		assertFalse(ClassUtils.isAssignable(Integer.class, Number.class));
		assertFalse(ClassUtils.isAssignable(Integer.class, double.class));
		assertFalse(ClassUtils.isAssignable(double.class, Integer.class));
	}
	
	@Test
	public void testClassPackageAsResourcePath() {
		String result = ClassUtils.classPackageAsResourcePath(Proxy.class);
		assertTrue(result.equals("java/lang/reflect"));
	}
	
	@Test
	public void testAddResourcePathToPackagePath() {
		String result = "java/lang/reflect/xyzabc.xml";
		assertEquals(result, ClassUtils.addResourcePathToPackagePath(Proxy.class, "xyzabc.xml"));
		assertEquals(result, ClassUtils.addResourcePathToPackagePath(Proxy.class, "/xyzabc.xml"));
		
		assertEquals("java/lang/reflect/a/b/c/d.xml", ClassUtils.addResourcePathToPackagePath(Proxy.class, "a/b/c/d.xml"));
	}
	
	@Test
	public void testGetAllInterfaces() {
		DerivedTestBean testBean = new DerivedTestBean();
		List<Class<?>> ifcs = Arrays.asList(ClassUtils.getAllInterfaces(testBean));
		assertEquals(7, ifcs.size());
		
		assertTrue(ifcs.contains(Serializable.class));
		assertTrue(ifcs.contains(ITestBean.class));
		assertTrue(ifcs.contains(IOther.class));
	}
	
	@Test
	public void testClassNamesToString() {
		List<Class<?>> ifcs = new LinkedList<Class<?>>();
		ifcs.add(Serializable.class);
		ifcs.add(Runnable.class);
		assertEquals("[interface java.io.Serializable, interface java.lang.Runnable]", ifcs.toString());
		assertEquals("[java.io.Serializable, java.lang.Runnable]", ClassUtils.classNamesToString(ifcs));
		
		List<Class<?>> classes = new LinkedList<Class<?>>();
		classes.add(LinkedList.class);
		classes.add(Integer.class);
		assertEquals("[class java.util.LinkedList, class java.lang.Integer]", classes.toString());
		assertEquals("[java.util.LinkedList, java.lang.Integer]", ClassUtils.classNamesToString(classes));
		
		assertEquals("[interface java.util.List]", Collections.singletonList(List.class).toString());
		assertEquals("[java.util.List]", ClassUtils.classNamesToString(List.class));
		
		assertEquals("[]", Collections.EMPTY_LIST.toString());
		assertEquals("[]", ClassUtils.classNamesToString(Collections.EMPTY_LIST));
	}
	
	@Test
	public void testIsPrimitive() {
		assertTrue(ClassUtils.isPrimitiveArray(boolean[].class));
		assertFalse(ClassUtils.isPrimitiveArray(Object[].class));
		assertFalse(ClassUtils.isPrimitiveArray(String[].class));
	}
	
	@Test
	public void testIsPrimitiveOrWrapper() {
		assertTrue(ClassUtils.isPrimitiveOrWrapper(boolean.class));
		assertTrue(ClassUtils.isPrimitiveOrWrapper(Boolean.class));
		assertTrue(ClassUtils.isPrimitiveOrWrapper(char.class));
		assertTrue(ClassUtils.isPrimitiveOrWrapper(Character.class));
		assertTrue(ClassUtils.isPrimitiveOrWrapper(float.class));
		assertTrue(ClassUtils.isPrimitiveOrWrapper(Float.class));
		assertTrue(ClassUtils.isPrimitiveOrWrapper(double.class));
		assertTrue(ClassUtils.isPrimitiveOrWrapper(Double.class));
		assertTrue(ClassUtils.isPrimitiveOrWrapper(byte.class));
		assertTrue(ClassUtils.isPrimitiveOrWrapper(Byte.class));
		assertTrue(ClassUtils.isPrimitiveOrWrapper(short.class));
		assertTrue(ClassUtils.isPrimitiveOrWrapper(Short.class));
		assertTrue(ClassUtils.isPrimitiveOrWrapper(int.class));
		assertTrue(ClassUtils.isPrimitiveOrWrapper(Integer.class));
		assertTrue(ClassUtils.isPrimitiveOrWrapper(long.class));
		assertTrue(ClassUtils.isPrimitiveOrWrapper(Long.class));
		
		assertFalse(ClassUtils.isPrimitiveOrWrapper(Object.class));
	}
	
	//----------------------------------------------------------
	//
	//----------------------------------------------------------
	
	public static class InnerClass {
		
		static boolean noArgCalled;
		static boolean argCalled;
		static boolean overloadedCalled;
		
		public static void staticMethod() {
			noArgCalled = true;
		}
		public static void staticMethod(String anArg) {
			overloadedCalled = true;
		}
		public static void argStaticMethod(String anArg) {
			argCalled = true;
		}
	}
	
	@SuppressWarnings("unused")
	private static class OverloadedMethodsClass {
		
		public void print(String message) {
			
		}
		
		public void print(String[] message) {
			
		}
	}
	
	@SuppressWarnings("unused")
	private static class SubOverloadedMethodsClass extends OverloadedMethodsClass {
		
		public void print(String[] message) {
			
		}
		
		public void print(String header, String[] message) {
			
		}
		
		protected void print(String header, String[] message, String footer) {
			
		}
		
		private void print(String[] message, String footer) {
			
		}
	}
}
