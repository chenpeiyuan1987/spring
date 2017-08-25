package org.yuan.study.spring.beans;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.fail;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.junit.Test;

public final class BeanWrapperTest {

	@Test
	public void testIsReadablePropertyWithNotReadable() {
		BeanWrapper bw = new BeanWrapperImpl(new NoRead());
		assertFalse(bw.isReadableProperty("age"));
	}
	
	@Test
	public void testIsReadablePropertyWithNoSuchProperty() {
		BeanWrapper bw = new BeanWrapperImpl(new NoRead());
		assertFalse(bw.isReadableProperty("xxx"));
	}
	
	@Test
	public void testIsReadablePropertyWithNull() {
		BeanWrapper bw = new BeanWrapperImpl(new NoRead());
		try {
			bw.isReadableProperty(null);
			fail();
		}
		catch (IllegalArgumentException ex) {}
	}
	
	@Test
	public void testIsWritablePropertyWithNull() {
		BeanWrapper bw = new BeanWrapperImpl(new NoRead());
		try {
			bw.isWritableProperty(null);
			fail();
		}
		catch (IllegalArgumentException ex) {}
	}
	
	@Test
	public void testSetPropertyValue() {
		Getter getter = new Getter();
		BeanWrapper bw = new BeanWrapperImpl(getter);
		bw.setPropertyValue("name", "tom");
		assertEquals("tom", getter.getName());
	}
	
	@Test
	public void testNumberObject() {
		NumberTest nt = new NumberTest();
		BeanWrapper bw = new BeanWrapperImpl(nt);
		
		try {
			bw.setPropertyValue("int2", "2");
			bw.setPropertyValue("long2", "3");
			bw.setPropertyValue("short2", "4");
			bw.setPropertyValue("float2", "5.1");
			bw.setPropertyValue("double2", "6.1");
			bw.setPropertyValue("bigInteger", "7");
			bw.setPropertyValue("bigDecimal", "8.1");
		}
		catch (BeansException ex) {
			fail();
		}
		
		assertEquals(new Integer(2), bw.getPropertyValue("int2"));
		assertEquals(new Integer(2), nt.getInt2());
		assertEquals(new Long(3), bw.getPropertyValue("long2"));
		assertEquals(new Long(3), nt.getLong2());
		assertEquals(new Short("4"), bw.getPropertyValue("short2"));
		assertEquals(new Short("4"), nt.getShort2());
		assertEquals(new Float(5.1), bw.getPropertyValue("float2"));
		assertEquals(new Float(5.1), nt.getFloat2());
		assertEquals(new Double(6.1), bw.getPropertyValue("double2"));
		assertEquals(new Double(6.1), nt.getDouble2());
		assertEquals(new BigInteger("7"), bw.getPropertyValue("bigInteger"));
		assertEquals(new BigInteger("7"), nt.getBigInteger());
		assertEquals(new BigDecimal("8.1"), bw.getPropertyValue("bigDecimal"));
		assertEquals(new BigDecimal("8.1"), nt.getBigDecimal());
	}
	
	//------------------------------------------------------
	// Private static class for test use
	//------------------------------------------------------
	
	private static class NoRead {
		
	}
	
	private static class Getter {
		
		private String name;

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}
		
	}
	
	private static class NumberTest {
		private short short1;
		private int int1;
		private long long1;
		private float float1;
		private double double1;
		private BigInteger bigInteger;
		private BigDecimal bigDecimal;
		
		private Short short2;
		private Integer int2;
		private Long long2;
		private Float float2;
		private Double double2;
		
		public short getShort1() {
			return short1;
		}
		public void setShort1(short short1) {
			this.short1 = short1;
		}
		public int getInt1() {
			return int1;
		}
		public void setInt1(int int1) {
			this.int1 = int1;
		}
		public long getLong1() {
			return long1;
		}
		public void setLong1(long long1) {
			this.long1 = long1;
		}
		public float getFloat1() {
			return float1;
		}
		public void setFloat1(float float1) {
			this.float1 = float1;
		}
		public double getDouble1() {
			return double1;
		}
		public void setDouble1(double double1) {
			this.double1 = double1;
		}
		public BigInteger getBigInteger() {
			return bigInteger;
		}
		public void setBigInteger(BigInteger bigInteger) {
			this.bigInteger = bigInteger;
		}
		public BigDecimal getBigDecimal() {
			return bigDecimal;
		}
		public void setBigDecimal(BigDecimal bigDecimal) {
			this.bigDecimal = bigDecimal;
		}
		public Short getShort2() {
			return short2;
		}
		public void setShort2(Short short2) {
			this.short2 = short2;
		}
		public Integer getInt2() {
			return int2;
		}
		public void setInt2(Integer int2) {
			this.int2 = int2;
		}
		public Long getLong2() {
			return long2;
		}
		public void setLong2(Long long2) {
			this.long2 = long2;
		}
		public Float getFloat2() {
			return float2;
		}
		public void setFloat2(Float float2) {
			this.float2 = float2;
		}
		public Double getDouble2() {
			return double2;
		}
		public void setDouble2(Double double2) {
			this.double2 = double2;
		}
		
	}
}
