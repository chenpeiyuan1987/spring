package org.yuan.study.spring.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

import org.junit.Test;

public class ClassUtilsTest {

	private ClassLoader classLoader = getClass().getClassLoader();
	
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
		assertEquals(boolean.class, ClassUtils.forName("boolean", classLoader));
		assertEquals(char.class, ClassUtils.forName("char", classLoader));
		assertEquals(byte.class, ClassUtils.forName("byte", classLoader));
		assertEquals(short.class, ClassUtils.forName("short", classLoader));
		assertEquals(int.class, ClassUtils.forName("int", classLoader));
		assertEquals(long.class, ClassUtils.forName("long", classLoader));
		assertEquals(float.class, ClassUtils.forName("float", classLoader));
		assertEquals(double.class, ClassUtils.forName("double", classLoader));
		assertEquals(void.class, ClassUtils.forName("void", classLoader));
	}
	
	@Test
	public void testForNameWithPrimitiveArrays() throws ClassNotFoundException {
		assertEquals(boolean[].class, ClassUtils.forName("boolean[]", classLoader));
		assertEquals(char[].class, ClassUtils.forName("char[]", classLoader));
		assertEquals(byte[].class, ClassUtils.forName("byte[]", classLoader));
		assertEquals(short[].class, ClassUtils.forName("short[]", classLoader));
		assertEquals(int[].class, ClassUtils.forName("int[]", classLoader));
		assertEquals(long[].class, ClassUtils.forName("long[]", classLoader));
		assertEquals(float[].class, ClassUtils.forName("float[]", classLoader));
		assertEquals(double[].class, ClassUtils.forName("double[]", classLoader));
	}
	
	@Test
	public void testForNameWithPrimitiveArraysInternalName() throws ClassNotFoundException {
		assertEquals(boolean[].class, ClassUtils.forName(boolean[].class.getName(), classLoader));
		assertEquals(char[].class, ClassUtils.forName(char[].class.getName(), classLoader));
		assertEquals(byte[].class, ClassUtils.forName(byte[].class.getName(), classLoader));
		assertEquals(short[].class, ClassUtils.forName(short[].class.getName(), classLoader));
		assertEquals(int[].class, ClassUtils.forName(int[].class.getName(), classLoader));
		assertEquals(long[].class, ClassUtils.forName(long[].class.getName(), classLoader));
		assertEquals(float[].class, ClassUtils.forName(float[].class.getName(), classLoader));
		assertEquals(double[].class, ClassUtils.forName(double[].class.getName(), classLoader));
	}
	
	@Test
	public void testGetShortName() {
		String className = ClassUtils.getShortName(getClass());
		assertEquals("ClassUtilsTest", className);
	}
	
	@Test
	public void testGetMethodCountForName() {
		assertEquals(2, ClassUtils.getMethodCountForName(OverloadedMethodsClass.class, "print"));
		assertEquals(6, ClassUtils.getMethodCountForName(SubOverloadedMethodsClass.class, "print"));
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
