package org.yuan.study.spring.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.sql.SQLException;

import org.junit.Test;

public final class ObjectUtilsTest {

	@Test
	public void testIsCheckedException() {
		assertTrue(ObjectUtils.isCheckedException(new Exception()));
		assertTrue(ObjectUtils.isCheckedException(new SQLException()));
		assertTrue(ObjectUtils.isCheckedException(new Throwable()));
		
		assertFalse(ObjectUtils.isCheckedException(new RuntimeException()));
		assertFalse(ObjectUtils.isCheckedException(new SubRuntimeException()));
	}
	
	@Test
	public void testIsCompatibleWithThrowsClause() {
		
	}
	
	@Test
	public void testToObjectArray() {
		int[] a = new int[] {1, 2, 3, 4, 5};
		Integer[] w = (Integer[]) ObjectUtils.toObjectArray(a);
		assertTrue(w.length == 5);
		for (int i = 0; i < w.length; i++) {
			assertEquals(a[i], w[i].intValue());
		}
	}
	
	@Test
	public void testToObjectArrayWithNull() {
		Object[] objects = ObjectUtils.toObjectArray(null);
		assertTrue(objects != null);
		assertEquals(0, objects.length);
	}
	
	@Test
	public void testToObjectArrayWithEmptyPrimitiveArray() {
		Object[] objects = ObjectUtils.toObjectArray(new byte[] {});
		assertTrue(objects != null);
		assertEquals(0, objects.length);
	}
	
	@Test
	public void testToObjectArrayWithNonArrayType() {
		try {
			ObjectUtils.toObjectArray("Not an []");
			fail();
		} 
		catch (IllegalArgumentException ex) {}
	}
	
	@Test
	public void testToObjectArrayWithNonPrimitiveArray() {
		String[] source = new String[] {"Bingo"};
		assertTrue(source == ObjectUtils.toObjectArray(source));
	}
	
	@Test
	public void testAddObjectToArraySunnyDay() {
		String element = "baz";
		Object[] array = ObjectUtils.addObjectToArray(new String[] {"foo", "bar"}, element);
		assertEquals(3, array.length);
		assertEquals(element, array[2]);
	}
	
	@Test
	public void testAddObjectToArrayWhenEmpty() {
		String element = "foo";
		Object[] array = ObjectUtils.addObjectToArray(new String[0], element);
		assertEquals(1, array.length);
		assertEquals(element, array[0]);
	}
	
	@Test
	public void testAddObjectToSingleNonNullElementArray() {
		
	}
	
	private static class SubRuntimeException extends RuntimeException {
		private static final long serialVersionUID = 1L;
	}
}
