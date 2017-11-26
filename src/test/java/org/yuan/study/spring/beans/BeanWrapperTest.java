package org.yuan.study.spring.beans;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.junit.Test;
import org.yuan.study.spring.beans.propertyeditors.StringTrimmerEditor;

import test.beans.IndexedTestBean;
import test.beans.NumberTestBean;
import test.beans.TestBean;

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
	public void testReadableAndWritableForIndexedProperties() {
		BeanWrapper bw = new BeanWrapperImpl(IndexedTestBean.class);
		
		assertTrue(bw.isReadableProperty("array"));
		assertTrue(bw.isReadableProperty("list"));
		assertTrue(bw.isReadableProperty("set"));
		assertTrue(bw.isReadableProperty("map"));
		assertFalse(bw.isReadableProperty("xxx"));
		
		assertTrue(bw.isWritableProperty("array"));
		assertTrue(bw.isWritableProperty("list"));
		assertTrue(bw.isWritableProperty("set"));
		assertTrue(bw.isWritableProperty("map"));
		assertFalse(bw.isWritableProperty("xxx"));
		
		assertTrue(bw.isReadableProperty("array[0]"));
		assertTrue(bw.isReadableProperty("array[0].name"));
		assertTrue(bw.isReadableProperty("list[0]"));
		assertTrue(bw.isReadableProperty("list[0].name"));
		assertTrue(bw.isReadableProperty("set[0]"));
		assertTrue(bw.isReadableProperty("set[0].name"));
		assertTrue(bw.isReadableProperty("map[key1]"));
		assertTrue(bw.isReadableProperty("map[key1].name"));
		assertTrue(bw.isReadableProperty("map[key4][0]"));
		assertTrue(bw.isReadableProperty("map[key4][0].name"));
		assertTrue(bw.isReadableProperty("map[key4][1]"));
		assertTrue(bw.isReadableProperty("map[key4][1].name"));
		assertFalse(bw.isReadableProperty("array[key1]"));
		
		assertTrue(bw.isWritableProperty("array[0]"));
		assertTrue(bw.isWritableProperty("array[0].name"));
		assertTrue(bw.isWritableProperty("list[0]"));
		assertTrue(bw.isWritableProperty("list[0].name"));
		assertTrue(bw.isWritableProperty("set[0]"));
		assertTrue(bw.isWritableProperty("set[0].name"));
		assertTrue(bw.isWritableProperty("map[key1]"));
		assertTrue(bw.isWritableProperty("map[key1].name"));
		assertTrue(bw.isWritableProperty("map[key4][0]"));
		assertTrue(bw.isWritableProperty("map[key4][0].name"));
		assertTrue(bw.isWritableProperty("map[key4][1]"));
		assertTrue(bw.isWritableProperty("map[key4][1].name"));
		assertFalse(bw.isWritableProperty("array[key1]"));
	}
	
	@Test
	public void testTypeDeterminationForIndexedProperty() {
		BeanWrapper bw = new BeanWrapperImpl(IndexedTestBean.class);
		assertEquals(null, bw.getPropertyType("map[key0]"));
		
		bw = new BeanWrapperImpl(IndexedTestBean.class);
		bw.setPropertyValue("map[key0]", "my String");
		assertEquals(String.class, bw.getPropertyType("map[key0]"));
		
		bw = new BeanWrapperImpl(IndexedTestBean.class);
		bw.registerCustomEditor(String.class, "map[key0]", new StringTrimmerEditor(false));
		assertEquals(String.class, bw.getPropertyType("map[key0]"));
	}
	
	@Test
	public void testGetterThrowsException() {
		Getter getter = new Getter();
		BeanWrapper bw = new BeanWrapperImpl(getter);
		bw.setPropertyValue("name", "tom");
		assertTrue(getter.getName().equals("tom"));
	}
	
	@Test
	public void testEmptyPropertyValuesSet() {
		TestBean t = new TestBean();
		int age = 30;
		String name = "chen";
		t.setAge(age);
		t.setName(name);
		
		BeanWrapper bw = new BeanWrapperImpl(t);
		assertTrue(t.getAge() == age);
		assertTrue(name.equals(t.getName()));
		
		bw.setPropertyValues(new MutablePropertyValues());
		assertTrue(t.getAge() == age);
		assertTrue(name.equals(t.getName()));
	}
	
	@Test
	public void testAllValid() {
		TestBean t = new TestBean();
		int age = 65;
		String name = "tony";
		String touchy = "valid";
		
		BeanWrapper bw = new BeanWrapperImpl(t);
		MutablePropertyValues pvs = new MutablePropertyValues();
		pvs.addPropertyValue("age", age);
		pvs.addPropertyValue("name", name);
		pvs.addPropertyValue("touchy", touchy);
		bw.setPropertyValues(pvs);
		
		assertEquals(age, t.getAge());
		assertEquals(name, t.getName());
		assertEquals(touchy, t.getTouchy());
	}
	
	@Test
	public void testBeanWrapperUpdates() {
		TestBean t = new TestBean();
		int age = 33;
		t.setAge(age);
		
		BeanWrapper bw = new BeanWrapperImpl(t);
		Object obj = bw.getPropertyValue("age");
		assertTrue(obj instanceof Integer);
		int tmp = ((Integer)obj).intValue();
		assertEquals(age, tmp);
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
		NumberTestBean nt = new NumberTestBean();
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
		
		public void setAge(int age) {}
		
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
	
	private static class EnumTester {
		private Autowire autowire;

		public Autowire getAutowire() {
			return autowire;
		}

		public void setAutowire(Autowire autowire) {
			this.autowire = autowire;
		}
		
	}
	
}
