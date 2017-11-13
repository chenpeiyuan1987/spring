package org.yuan.study.spring.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
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
		Class[] empty = new Class[0];
		Class[] exception = new Class[] {Exception.class};
		Class[] sqlAndIO = new Class[] {SQLException.class, IOException.class};
		Class[] throwable = new Class[] {Throwable.class};
		
		assertTrue(ObjectUtils.isCompatibleWithThrowsClause(new RuntimeException(), null));
		assertTrue(ObjectUtils.isCompatibleWithThrowsClause(new RuntimeException(), empty));
		assertTrue(ObjectUtils.isCompatibleWithThrowsClause(new RuntimeException(), exception));
		assertTrue(ObjectUtils.isCompatibleWithThrowsClause(new RuntimeException(), sqlAndIO));
		assertTrue(ObjectUtils.isCompatibleWithThrowsClause(new RuntimeException(), throwable));
		
		assertFalse(ObjectUtils.isCompatibleWithThrowsClause(new Exception(), null));
		assertFalse(ObjectUtils.isCompatibleWithThrowsClause(new Exception(), empty));
		assertFalse(ObjectUtils.isCompatibleWithThrowsClause(new Exception(), sqlAndIO));
		assertTrue(ObjectUtils.isCompatibleWithThrowsClause(new Exception(), exception));
		assertTrue(ObjectUtils.isCompatibleWithThrowsClause(new Exception(), throwable));
		
		assertFalse(ObjectUtils.isCompatibleWithThrowsClause(new SQLException(), null));
		assertFalse(ObjectUtils.isCompatibleWithThrowsClause(new SQLException(), empty));
		assertTrue(ObjectUtils.isCompatibleWithThrowsClause(new SQLException(), exception));
		assertTrue(ObjectUtils.isCompatibleWithThrowsClause(new SQLException(), sqlAndIO));
		assertTrue(ObjectUtils.isCompatibleWithThrowsClause(new SQLException(), throwable));

		assertFalse(ObjectUtils.isCompatibleWithThrowsClause(new Throwable(), null));
		assertFalse(ObjectUtils.isCompatibleWithThrowsClause(new Throwable(), empty));
		assertFalse(ObjectUtils.isCompatibleWithThrowsClause(new Throwable(), exception));
		assertFalse(ObjectUtils.isCompatibleWithThrowsClause(new Throwable(), sqlAndIO));
		assertTrue(ObjectUtils.isCompatibleWithThrowsClause(new Throwable(), throwable));
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
		Object[] array = ObjectUtils.addObjectToArray(new String[] {"foo"}, "bar");
		assertEquals(2, array.length);
		assertEquals("foo", array[0]);
		assertEquals("bar", array[1]);
	}
	
	@Test
	public void testAddObjectToSingleNullElementArray() {
		Object[] array = ObjectUtils.addObjectToArray(null, "foo");
		
		assertEquals(1, array.length);
		assertEquals("foo", array[0]);
	}
	
	@Test
	public void testAddNullObjectToNullArray() {
		Object[] array = ObjectUtils.addObjectToArray(null, null);
		
		assertEquals(1, array.length);
		assertEquals(null, array[0]);
	}
	
	@Test
	public void testNullSafeEqualsWithArrays() {
		assertTrue(ObjectUtils.nullSafeEquals(new String[] {"a", "b", "c"}, new String[] {"a", "b", "c"}));
		assertTrue(ObjectUtils.nullSafeEquals(new int[] {1, 2, 3}, new int[] {1, 2, 3}));
	}
	
	@Test
	public void testHashCodeWithBoolean() {
		assertEquals(Boolean.FALSE.hashCode(), ObjectUtils.hashCode(false));
		assertEquals(Boolean.TRUE.hashCode(), ObjectUtils.hashCode(true));
	}
	
	@Test
	public void testHashCodeWithDouble() {
		double db1 = 9830.43;
		assertEquals(new Double(db1).hashCode(), ObjectUtils.hashCode(db1));
	}
	
	@Test
	public void testHashCodeWithFloat() {
		float flt = 34.8f;
		assertEquals(new Float(flt).hashCode(), ObjectUtils.hashCode(flt));
	}
	
	@Test
	public void testHashCodeWithLong() {
		long lng = 8831;
		assertEquals(new Long(8831).hashCode(), ObjectUtils.hashCode(lng));
	}
	
	@Test
	public void testIdentityToString() {
		Object obj = new Object();
		String expect = obj.getClass().getName() + "@" + ObjectUtils.getIdentityHexString(obj);
		String actual = ObjectUtils.identityToString(obj);
		assertEquals(expect.toString(), actual);
	}
	
	@Test
	public void testIdentityToStringWithNullObject() {
		assertEquals("", ObjectUtils.identityToString(null));
	}
	
	@Test
	public void testNullSafeHashCodeWithBooleanArray() {
		int expect = 31 * 7 + Boolean.TRUE.hashCode();
		expect = 31 * expect + Boolean.FALSE.hashCode();
		
		boolean[] array = {true, false};
		int actual = ObjectUtils.nullSafeHashCode(array);
		assertEquals(expect, actual);
		
		assertEquals(0, ObjectUtils.nullSafeHashCode((boolean[]) null));
	}
	
	@Test
	public void testNullSafeHashCodeWithByteArray() {
		int expect = 31 * 7 + 8;
		expect = 31 * expect + 10;
		
		byte[] array = {8, 10};
		int actual = ObjectUtils.nullSafeHashCode(array);
		assertEquals(expect, actual);
		
		assertEquals(0, ObjectUtils.nullSafeHashCode((byte[]) null));
	}
	
	@Test
	public void testNullSafeHashCodeWithCharArray() {
		int expect = 31 * 7 + 'a';
		expect = 31 * expect + 'E';
		
		char[] array = {'a', 'E'};
		int actual = ObjectUtils.nullSafeHashCode(array);
		assertEquals(expect, actual);
		
		assertEquals(0, ObjectUtils.nullSafeHashCode((char[]) null));
	}
	
	@Test
	public void testNullSafeHashCodeWithDoubleArray() {
		long bits = Double.doubleToLongBits(8449.65);
		int expect = 31 * 7 + (int)(bits ^ (bits >>> 32));
		bits = Double.doubleToLongBits(9944.923);
		expect = 31 * expect + (int)(bits ^ (bits >>> 32));
		
		double[] array = {8449.65, 9944.923};
		int actual = ObjectUtils.nullSafeHashCode(array);
		assertEquals(expect, actual);
		
		assertEquals(0, ObjectUtils.nullSafeHashCode((double[]) null));
	}
	
	@Test
	public void testNullSafeHashCodeWithFloatArray() {
		int expect = 31 * 7 + Float.floatToIntBits(9.6f);
		expect = 31 * expect + Float.floatToIntBits(7.4f);
		
		float[] array = {9.6f, 7.4f};
		int actual = ObjectUtils.nullSafeHashCode(array);
		assertEquals(expect, actual);
		
		assertEquals(0, ObjectUtils.nullSafeHashCode((float[]) null));
	}
	
	@Test
	public void testNullSafeHashCodeWithIntArray() {
		int expect = 31 * 7 + 884;
		expect = 31 * expect + 340;
		
		int[] array = {884, 340};
		int actual = ObjectUtils.nullSafeHashCode(array);
		assertEquals(expect, actual);
		
		assertEquals(0, ObjectUtils.nullSafeHashCode((int[]) null));
	}
	
	@Test
	public void testNullSafeHashCodeWithShortArray() {
		int expect = 31 * 7 + 70;
		expect = 31 * expect + 8;
		
		short[] array = {70, 8};
		int actual = ObjectUtils.nullSafeHashCode(array);
		assertEquals(expect, actual);
		
		assertEquals(0, ObjectUtils.nullSafeHashCode((short[]) null));
	}
	
	@Test
	public void testNullSafeHashCodeWithLongArray() {
		long lng = 79931;
		int expect = 31 * 7 + (int)(lng ^ (lng >>> 32));
		lng = 843201;
		expect = 31 * expect + (int)(lng ^ (lng >>> 32));
		
		long[] array = {79931, 843201};
		int actual = ObjectUtils.nullSafeHashCode(array);
		assertEquals(expect, actual);
		
		assertEquals(0, ObjectUtils.nullSafeHashCode((long[]) null));
	}
	
	@Test
	public void testNullSafeHashCodeWithObject() {
		String str = "Luke";
		assertEquals(str.hashCode(), ObjectUtils.nullSafeHashCode(str));
	}
	
	@Test
	public void testNullSafeHashCodeWithObjectArray() {
		int expect = 31 * 7 + "Leia".hashCode();
		expect = 31 * expect + "Han".hashCode();
		
		Object[] array = {"Leia", "Han"};
		int actual = ObjectUtils.nullSafeHashCode(array);
		assertEquals(expect, actual);
		
		assertEquals(0, ObjectUtils.nullSafeHashCode((Object[]) null));
	}
	
	@Test
	public void testNullSafeHashCodeWithObjectBeingArray() {
		Object array = new boolean[] {true, false};
		int expect = ObjectUtils.nullSafeHashCode((boolean[]) array);
		assertEqualsHashCodes(expect, array);
		
		array = new byte[] {6, 39};
		expect = ObjectUtils.nullSafeHashCode((byte[]) array);
		assertEqualsHashCodes(expect, array);
		
		array = new char[] {'1', 'M'};
		expect = ObjectUtils.nullSafeHashCode((char[]) array);
		assertEqualsHashCodes(expect, array);
		
		array = new double[] {68930.993, 9022.009};
		expect = ObjectUtils.nullSafeHashCode((double[]) array);
		assertEqualsHashCodes(expect, array);
		
		array = new float[] {9.9f, 9.54f};
		expect = ObjectUtils.nullSafeHashCode((float[]) array);
		assertEqualsHashCodes(expect, array);
		
		array = new int[] {89, 32};
		expect = ObjectUtils.nullSafeHashCode((int[]) array);
		assertEqualsHashCodes(expect, array);
		
		array = new long[] {4389, 320};
		expect = ObjectUtils.nullSafeHashCode((long[]) array);
		assertEqualsHashCodes(expect, array);
		
		array = new short[] {5, 3};
		expect = ObjectUtils.nullSafeHashCode((short[]) array);
		assertEqualsHashCodes(expect, array);
		
		array = new Object[] {"Luke", "Anakin"};
		expect = ObjectUtils.nullSafeHashCode((Object[]) array);
		assertEqualsHashCodes(expect, array);
		
		assertEquals(0, ObjectUtils.nullSafeHashCode((Object)null));
	}
	
	@Test
	public void testNullSafeToString() {
		assertEquals("{true, false}", ObjectUtils.nullSafeToString(new boolean[]{true, false}));
		assertEquals("{}", ObjectUtils.nullSafeToString(new boolean[]{}));
		assertEquals("null", ObjectUtils.nullSafeToString((boolean[])null));
		
		assertEquals("{5, 8}", ObjectUtils.nullSafeToString(new byte[]{5, 8}));
		assertEquals("{}", ObjectUtils.nullSafeToString(new byte[]{}));
		assertEquals("null", ObjectUtils.nullSafeToString((byte[])null));
		
		assertEquals("{'A', 'B'}", ObjectUtils.nullSafeToString(new char[]{'A', 'B'}));
		assertEquals("{}", ObjectUtils.nullSafeToString(new char[]{}));
		assertEquals("null", ObjectUtils.nullSafeToString((char[])null));
		
		assertEquals("{8594.93, 8594023.95}", ObjectUtils.nullSafeToString(new double[]{8594.93, 8594023.95}));
		assertEquals("{}", ObjectUtils.nullSafeToString(new double[]{}));
		assertEquals("null", ObjectUtils.nullSafeToString((double[])null));
		
		assertEquals("{8.6, 43.8}", ObjectUtils.nullSafeToString(new float[]{8.6f, 43.8f}));
		assertEquals("{}", ObjectUtils.nullSafeToString(new float[]{}));
		assertEquals("null", ObjectUtils.nullSafeToString((float[])null));
		
		assertEquals("{9, 64}", ObjectUtils.nullSafeToString(new int[]{9, 64}));
		assertEquals("{}", ObjectUtils.nullSafeToString(new int[]{}));
		assertEquals("null", ObjectUtils.nullSafeToString((int[])null));
		
		assertEquals("{434, 23423}", ObjectUtils.nullSafeToString(new long[]{434l, 23423l}));
		assertEquals("{}", ObjectUtils.nullSafeToString(new long[]{}));
		assertEquals("null", ObjectUtils.nullSafeToString((long[])null));
		
		assertEquals("{7, 9}", ObjectUtils.nullSafeToString(new short[]{7, 9}));
		assertEquals("{}", ObjectUtils.nullSafeToString(new short[]{}));
		assertEquals("null", ObjectUtils.nullSafeToString((short[])null));
		
		assertEquals("{Han, 43}", ObjectUtils.nullSafeToString(new Object[]{"Han", new Long(43)}));
		assertEquals("{}", ObjectUtils.nullSafeToString(new Object[]{}));
		assertEquals("null", ObjectUtils.nullSafeToString((Object[])null));
		
		assertEquals("{Luke, Anakin}", ObjectUtils.nullSafeToString(new String[]{"Luke", "Anakin"}));
		assertEquals("{}", ObjectUtils.nullSafeToString(new String[]{}));
		assertEquals("null", ObjectUtils.nullSafeToString((String[])null));
		
		assertEquals("I shoh love tha taste of mangoes", ObjectUtils.nullSafeToString("I shoh love tha taste of mangoes"));
	}
	
	private void assertEqualsHashCodes(int expect, Object array) {
		int actual = ObjectUtils.nullSafeHashCode(array);
		assertEquals(expect, actual);
		assertTrue(array.hashCode() != actual);
	}
	
	private static class SubRuntimeException extends RuntimeException {
		private static final long serialVersionUID = 1L;
	}
}
