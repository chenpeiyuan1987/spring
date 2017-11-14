package org.yuan.study.spring.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.NumberFormat;
import java.util.Locale;

import org.junit.Test;
import org.yuan.study.spring.core.JdkVersion;

public class NumberUtilsTest {
	
	@Test
	public void testParseNumber() {
		assertEquals(new Byte(Byte.MAX_VALUE), NumberUtils.parseNumber(String.valueOf(Byte.MAX_VALUE), Byte.class));
		assertEquals(new Short(Short.MAX_VALUE), NumberUtils.parseNumber(String.valueOf(Short.MAX_VALUE), Short.class));
		assertEquals(new Integer(Integer.MAX_VALUE), NumberUtils.parseNumber(String.valueOf(Integer.MAX_VALUE), Integer.class));
		assertEquals(new Long(Long.MAX_VALUE), NumberUtils.parseNumber(String.valueOf(Long.MAX_VALUE), Long.class));
		assertEquals(new Float(Float.MAX_VALUE), NumberUtils.parseNumber(String.valueOf(Float.MAX_VALUE), Float.class));
		assertEquals(new Double(Double.MAX_VALUE), NumberUtils.parseNumber(String.valueOf(Double.MAX_VALUE), Double.class));
	}
	
	@Test
	public void testParseNumberUsingNumberFormat() {
		NumberFormat nf = NumberFormat.getNumberInstance(Locale.US);
		
		assertEquals(new Byte(Byte.MAX_VALUE), NumberUtils.parseNumber(String.valueOf(Byte.MAX_VALUE), Byte.class, nf));
		assertEquals(new Short(Short.MAX_VALUE), NumberUtils.parseNumber(String.valueOf(Short.MAX_VALUE), Short.class, nf));
		assertEquals(new Integer(Integer.MAX_VALUE), NumberUtils.parseNumber(String.valueOf(Integer.MAX_VALUE), Integer.class, nf));
		assertEquals(new Long(Long.MAX_VALUE), NumberUtils.parseNumber(String.valueOf(Long.MAX_VALUE), Long.class, nf));
		assertEquals(new Float(Float.MAX_VALUE), NumberUtils.parseNumber(String.valueOf(Float.MAX_VALUE), Float.class, nf));
		assertEquals(new Double(Double.MAX_VALUE), NumberUtils.parseNumber(String.valueOf(Double.MAX_VALUE), Double.class, nf));
	}
	
	@Test
	public void testParseWithTrim() {
		assertEquals(new Byte(Byte.MAX_VALUE), NumberUtils.parseNumber(" " + Byte.MAX_VALUE + " ", Byte.class));
		assertEquals(new Short(Short.MAX_VALUE), NumberUtils.parseNumber(" " + Short.MAX_VALUE + " ", Short.class));
		assertEquals(new Integer(Integer.MAX_VALUE), NumberUtils.parseNumber(" " + Integer.MAX_VALUE + " ", Integer.class));
		assertEquals(new Long(Long.MAX_VALUE), NumberUtils.parseNumber(" " + Long.MAX_VALUE + " ", Long.class));
		assertEquals(new Float(Float.MAX_VALUE), NumberUtils.parseNumber(" " + Float.MAX_VALUE + " ", Float.class));
		assertEquals(new Double(Double.MAX_VALUE), NumberUtils.parseNumber(" " + Double.MAX_VALUE + " ", Double.class));
	}
	
	@Test
	public void testParseWithTrimUsingNumberFormat() {
		NumberFormat nf = NumberFormat.getNumberInstance(Locale.US);
		
		assertEquals(new Byte(Byte.MAX_VALUE), NumberUtils.parseNumber(" " + Byte.MAX_VALUE + " ", Byte.class, nf));
		assertEquals(new Short(Short.MAX_VALUE), NumberUtils.parseNumber(" " + Short.MAX_VALUE + " ", Short.class, nf));
		assertEquals(new Integer(Integer.MAX_VALUE), NumberUtils.parseNumber(" " + Integer.MAX_VALUE + " ", Integer.class, nf));
		assertEquals(new Long(Long.MAX_VALUE), NumberUtils.parseNumber(" " + Long.MAX_VALUE + " ", Long.class, nf));
		assertEquals(new Float(Float.MAX_VALUE), NumberUtils.parseNumber(" " + Float.MAX_VALUE + " ", Float.class, nf));
		assertEquals(new Double(Double.MAX_VALUE), NumberUtils.parseNumber(" " + Double.MAX_VALUE + " ", Double.class, nf));
	}
	
	@Test
	public void testParseAsHex() {
		String str = "FEBD4E677898DFEBFFEE44";
		
		assertByteEquals("0x" + Integer.toHexString(Byte.MAX_VALUE));
		assertShortEquals("0x" + Integer.toHexString(Short.MAX_VALUE));
		assertIntegerEquals("0x" + Integer.toHexString(Integer.MAX_VALUE));
		assertLongEquals("0x" + Long.toHexString(Long.MAX_VALUE));
		assertEquals(new BigInteger(str, 16), NumberUtils.parseNumber("0x" + str, BigInteger.class));
	}
	
	@Test
	public void testParseNegativeHex() {
		String str = "FEBD4E677898DFEBFFEE44";
		
		assertNegativeByteEquals("-0x80");
		assertNegativeShortEquals("-0x8000");
		assertNegativeIntegerEquals("-0x80000000");
		assertNegativeLongEquals("-0x8000000000000000");
		assertEquals(new BigInteger(str, 16).negate(), NumberUtils.parseNumber("-0x" + str, BigInteger.class));
	}
	
	@Test
	public void testDoubleToBigInteger() {
		Double decimal = new Double(3.14d);
		assertEquals(new BigInteger("3"), NumberUtils.convertNumberToTargetClass(decimal, BigInteger.class));
	}
	
	@Test
	public void testBigDecimalToBigInteger() {
		String number = "987459837583750387355346";
		BigDecimal decimal = new BigDecimal(number);
		assertEquals(new BigInteger(number), NumberUtils.convertNumberToTargetClass(decimal, BigInteger.class));
	}
	
	@Test
	public void testNonExactBigDecimalToBigInteger() {
		BigDecimal decimal = new BigDecimal("987459837583750387355346.14");
		assertEquals(new BigInteger("987459837583750387355346"), NumberUtils.convertNumberToTargetClass(decimal, BigInteger.class));
	}
	
	@Test
	public void testParseBigDecimalNumber() {
		String bigDecimalAsString = "0.10";
		Number bigDecimal = NumberUtils.parseNumber(bigDecimalAsString, BigDecimal.class);
		assertEquals(new BigDecimal(bigDecimalAsString), bigDecimal);
		
		bigDecimalAsString = "0.001";
		bigDecimal = NumberUtils.parseNumber(bigDecimalAsString, BigDecimal.class);
		assertEquals(new BigDecimal(bigDecimalAsString), bigDecimal);
		
		bigDecimalAsString = "3.14159265358979323846";
		bigDecimal = NumberUtils.parseNumber(bigDecimalAsString, BigDecimal.class);
		assertEquals(new BigDecimal(bigDecimalAsString), bigDecimal);
	}
	
	@Test
	public void testParseLocalizedBigDecimalNumber() {
		if (JdkVersion.getMajorJavaVersion() < JdkVersion.JAVA_15) {
			return;
		}
		
		NumberFormat numberFormat = NumberFormat.getInstance(Locale.ENGLISH);
		
		String bigDecimalAsString = "0.10";
		Number bigDecimal = NumberUtils.parseNumber(bigDecimalAsString, BigDecimal.class, numberFormat);
		assertEquals(new BigDecimal(bigDecimalAsString), bigDecimal);
		
		bigDecimalAsString = "0.001";
		bigDecimal = NumberUtils.parseNumber(bigDecimalAsString, BigDecimal.class, numberFormat);
		assertEquals(new BigDecimal(bigDecimalAsString), bigDecimal);
		
		bigDecimalAsString = "3.14159265358979323846";
		bigDecimal = NumberUtils.parseNumber(bigDecimalAsString, BigDecimal.class, numberFormat);
		assertEquals(new BigDecimal(bigDecimalAsString), bigDecimal);
	}
	
	@Test
	public void testParseOverflow() {
		String aLong = String.valueOf(Long.MAX_VALUE);
		
		try {
			NumberUtils.parseNumber(aLong, Byte.class);
			fail();
		} catch (IllegalArgumentException e) {} 
		
		try {
			NumberUtils.parseNumber(aLong, Short.class);
			fail();
		} catch (IllegalArgumentException e) {} 
		
		try {
			NumberUtils.parseNumber(aLong, Integer.class);
			fail();
		} catch (IllegalArgumentException e) {} 
		
		assertEquals(new Long(Long.MAX_VALUE), NumberUtils.parseNumber(aLong, Long.class));
		assertEquals(new Double(Double.MAX_VALUE), NumberUtils.parseNumber(String.valueOf(Double.MAX_VALUE), Double.class));
	}
	
	@Test
	public void testParseNegativeOverflow() {
		String aLong = String.valueOf(Long.MIN_VALUE);
		
		try {
			NumberUtils.parseNumber(aLong, Byte.class);
			fail();
		} catch (IllegalArgumentException e) {} 
		
		try {
			NumberUtils.parseNumber(aLong, Short.class);
			fail();
		} catch (IllegalArgumentException e) {} 
		
		try {
			NumberUtils.parseNumber(aLong, Integer.class);
			fail();
		} catch (IllegalArgumentException e) {} 
		
		assertEquals(new Long(Long.MIN_VALUE), NumberUtils.parseNumber(aLong, Long.class));
		assertEquals(new Double(Double.MIN_VALUE), NumberUtils.parseNumber(String.valueOf(Double.MIN_VALUE), Double.class));
	}
	
	@Test
	public void testParseOverflowUsingNumberFormat() {
		NumberFormat nf = NumberFormat.getNumberInstance(Locale.US);
		String aLong = String.valueOf(Long.MAX_VALUE);
		
		try {
			NumberUtils.parseNumber(aLong, Byte.class, nf);
			fail();
		} catch (IllegalArgumentException e) {} 
		
		try {
			NumberUtils.parseNumber(aLong, Short.class, nf);
			fail();
		} catch (IllegalArgumentException e) {} 
		
		try {
			NumberUtils.parseNumber(aLong, Integer.class, nf);
			fail();
		} catch (IllegalArgumentException e) {} 
		
		assertEquals(new Long(Long.MAX_VALUE), NumberUtils.parseNumber(aLong, Long.class, nf));
		assertEquals(new Double(Double.MAX_VALUE), NumberUtils.parseNumber(String.valueOf(Double.MAX_VALUE), Double.class, nf));
	}
	
	@Test
	public void testParseNegativeOverflowUsingNumberFormat() {
		NumberFormat nf = NumberFormat.getNumberInstance(Locale.US);
		String aLong = String.valueOf(Long.MIN_VALUE);
		
		try {
			NumberUtils.parseNumber(aLong, Byte.class, nf);
			fail();
		} catch (IllegalArgumentException e) {} 
		
		try {
			NumberUtils.parseNumber(aLong, Short.class, nf);
			fail();
		} catch (IllegalArgumentException e) {} 
		
		try {
			NumberUtils.parseNumber(aLong, Integer.class, nf);
			fail();
		} catch (IllegalArgumentException e) {} 
		
		assertEquals(new Long(Long.MIN_VALUE), NumberUtils.parseNumber(aLong, Long.class, nf));
		assertEquals(new Double(Double.MIN_VALUE), NumberUtils.parseNumber(String.valueOf(Double.MIN_VALUE), Double.class, nf));
	}
	
	//-----------------------------------------------------------
	//
	//-----------------------------------------------------------
	
	private void assertByteEquals(String str) {
		assertEquals(Byte.MAX_VALUE, NumberUtils.parseNumber(str, Byte.class).byteValue());
	}
	private void assertShortEquals(String str) {
		assertEquals(Short.MAX_VALUE, NumberUtils.parseNumber(str, Short.class).shortValue());
	}
	private void assertIntegerEquals(String str) {
		assertEquals(Integer.MAX_VALUE, NumberUtils.parseNumber(str, Integer.class).intValue());
	}
	private void assertLongEquals(String str) {
		assertEquals(Long.MAX_VALUE, NumberUtils.parseNumber(str, Long.class).longValue());
	}
	private void assertNegativeByteEquals(String str) {
		assertEquals(Byte.MIN_VALUE, NumberUtils.parseNumber(str, Byte.class).byteValue());
	}
	private void assertNegativeShortEquals(String str) {
		assertEquals(Short.MIN_VALUE, NumberUtils.parseNumber(str, Short.class).shortValue());
	}
	private void assertNegativeIntegerEquals(String str) {
		assertEquals(Integer.MIN_VALUE, NumberUtils.parseNumber(str, Integer.class).intValue());
	}
	private void assertNegativeLongEquals(String str) {
		assertEquals(Long.MIN_VALUE, NumberUtils.parseNumber(str, Long.class).longValue());
	}
}
