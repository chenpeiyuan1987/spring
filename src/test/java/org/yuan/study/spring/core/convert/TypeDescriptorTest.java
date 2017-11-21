package org.yuan.study.spring.core.convert;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;

public class TypeDescriptorTest {
	
	public List<String> listOfString;
	
	public List<List<String>> listOfListOfString = new ArrayList<List<String>>();
	
	public List<List> listOfListOfUnknown = new ArrayList<List>();
	
	public int[] intArray;
	
	public List<String>[] arrayOfListOfString;
	
	public List<Integer> listField = new ArrayList<Integer>();
	
	public Map<String, Integer> mapField = new HashMap<String, Integer>();

	@Test
	public void testListDescriptor() throws Exception {
		TypeDescriptor typeDescriptor = new TypeDescriptor(TypeDescriptorTest.class.getDeclaredField("listOfString"));
		assertFalse(typeDescriptor.isArray());
		assertEquals(List.class, typeDescriptor.getType());
		assertEquals(String.class, typeDescriptor.getElementType());
		assertEquals("java.util.List<java.lang.String>", typeDescriptor.asString());
	}
	
	@Test
	public void testListOfListOfStringDescriptor() throws Exception {
		TypeDescriptor typeDescriptor = new TypeDescriptor(TypeDescriptorTest.class.getDeclaredField("listOfListOfString"));
		assertFalse(typeDescriptor.isArray());
		assertEquals(List.class, typeDescriptor.getType());
		assertEquals(List.class, typeDescriptor.getElementType());
		assertEquals(String.class, typeDescriptor.getElementTypeDescriptor().getElementType());
		assertEquals("java.util.List<java.util.List<java.lang.String>>", typeDescriptor.asString());
	}
	
	@Test
	public void testListOfListOfUnknownDescriptor() throws Exception {
		TypeDescriptor typeDescriptor = new TypeDescriptor(TypeDescriptorTest.class.getDeclaredField("listOfListOfUnknown"));
		assertFalse(typeDescriptor.isArray());
		assertEquals(List.class, typeDescriptor.getType());
		assertEquals(List.class, typeDescriptor.getElementType());
		assertEquals(Object.class, typeDescriptor.getElementTypeDescriptor().getElementType());
		assertEquals("java.util.List<java.util.List<java.lang.Object>>", typeDescriptor.asString());
	}
	
	@Test
	public void testArrayTypeDescriptor() throws Exception {
		TypeDescriptor typeDescriptor = new TypeDescriptor(TypeDescriptorTest.class.getDeclaredField("intArray"));
		assertTrue(typeDescriptor.isArray());
		assertEquals(Integer.TYPE, typeDescriptor.getElementType());
		assertEquals("int[]", typeDescriptor.asString());
	}
	
	@Test
	public void testBuildingArrayTypeDescriptor() throws Exception {
		TypeDescriptor typeDescriptor = TypeDescriptor.valueOf(int[].class);
		assertTrue(typeDescriptor.isArray());
		assertEquals(Integer.TYPE, typeDescriptor.getElementType());
	}
	
	@Test
	public void testComplexTypeDescriptor() throws Exception {
		TypeDescriptor typeDescriptor = new TypeDescriptor(TypeDescriptorTest.class.getDeclaredField("arrayOfListOfString"));
		assertTrue(typeDescriptor.isArray());
		assertEquals(List.class, typeDescriptor.getElementType());
		assertEquals("java.util.List[]", typeDescriptor.asString());
	}
	
	@Test
	public void testEquals() throws Exception {
		assertEquals(TypeDescriptor.valueOf(String.class), TypeDescriptor.valueOf(String.class));
		assertEquals(TypeDescriptor.valueOf(Date.class), TypeDescriptor.valueOf(Date.class));
		assertEquals(TypeDescriptor.valueOf(List.class), TypeDescriptor.valueOf(List.class));
		assertEquals(TypeDescriptor.valueOf(Map.class), TypeDescriptor.valueOf(Map.class));
		assertEquals(new TypeDescriptor(getClass().getField("listField")), new TypeDescriptor(getClass().getField("listField")));
		assertEquals(new TypeDescriptor(getClass().getField("mapField")), new TypeDescriptor(getClass().getField("mapField")));
	}
}
