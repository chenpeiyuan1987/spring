package org.yuan.study.spring.beans;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.beans.PropertyEditorSupport;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Properties;

import org.junit.Test;
import org.yuan.study.spring.beans.factory.annotation.Autowire;
import org.yuan.study.spring.beans.propertyeditors.StringTrimmerEditor;

import test.beans.BooleanTestBean;
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
	public void testValidNullUpdate() {
		TestBean tb = new TestBean();
		tb.setName("Frank");
		tb.setSpouse(tb);
		
		BeanWrapper bw = new BeanWrapperImpl(tb);
		assertTrue(tb.getName() != null);
		bw.setPropertyValue("name", null);
		assertTrue(tb.getName() == null);
		
		assertTrue(tb.getSpouse() != null);
		bw.setPropertyValue("spouse", null);
		assertTrue(tb.getSpouse() == null);
	}
	
	@Test
	public void testIgnoringIndexedProperty() {
		MutablePropertyValues values = new MutablePropertyValues();
		values.add("toBeIgnored[0]", new Integer(42));
		BeanWrapper bw = new BeanWrapperImpl(new Object());
		bw.setPropertyValues(values, true);
	}
	
	@Test
	public void testConvertPrimitiveToString() {
		MutablePropertyValues values = new MutablePropertyValues();
		values.add("name", new Integer(42));
		TestBean tb = new TestBean();
		BeanWrapper bw = new BeanWrapperImpl(tb);
		bw.setPropertyValues(values);
		assertEquals("42", tb.getName());
	}
	
	@Test
	public void testConvertClassToString() {
		MutablePropertyValues values = new MutablePropertyValues();
		values.add("name", Integer.class);
		TestBean tb = new TestBean();
		BeanWrapper bw = new BeanWrapperImpl(tb);
		bw.registerCustomEditor(String.class, new PropertyEditorSupport() {
			public void setValue(Object value) {
				super.setValue(value.toString());
			}
		});
		bw.setPropertyValues(values);
		assertEquals(Integer.class.toString(), tb.getName());
	}
	
	@Test
	public void testBooleanObject() {
		BooleanTestBean bt = new BooleanTestBean();
		BeanWrapper bw = new BeanWrapperImpl(bt);
		
		bw.setPropertyValue("bool2", "true");
		assertTrue(Boolean.TRUE.equals(bw.getPropertyValue("bool2")));
		assertTrue(bt.getBool2().booleanValue());
		
		bw.setPropertyValue("bool2", "false");
		assertTrue(Boolean.FALSE.equals(bw.getPropertyValue("bool2")));
		assertTrue(!bt.getBool2().booleanValue());
	}
	
	@Test
	public void testNumberObjects() {
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
	
	@Test
	public void testNumberCoercion() {
		NumberTestBean nt = new NumberTestBean();
		BeanWrapper bw = new BeanWrapperImpl(nt);
		
		try {
			bw.setPropertyValue("int2", new Long(2));
			bw.setPropertyValue("long2", new BigInteger("3"));
			bw.setPropertyValue("short2", new Integer(4));
			bw.setPropertyValue("float2", new Double(5.1));
			bw.setPropertyValue("double2", new BigDecimal(6.1));
			bw.setPropertyValue("bigInteger", new Integer(7));
			bw.setPropertyValue("bigDecimal", new Float(8.1));
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
	
	@Test
	public void testEnumByFieldName() {
		EnumTester et = new EnumTester();
		BeanWrapper bw = new BeanWrapperImpl(et);
		
		bw.setPropertyValue("autowire", "BY_NAME");
		assertEquals(Autowire.BY_NAME, et.getAutowire());
		
		bw.setPropertyValue("autowire", "  BY_TYPE ");
		assertEquals(Autowire.BY_TYPE, et.getAutowire());
		
		try {
			bw.setPropertyValue("autowire", "NHERITED");
			fail();
		} catch (Exception e) {}
	}
	
	@Test
	public void testPropertiesProperty() throws Exception {
		PropsTester pt = new PropsTester();
		BeanWrapper bw = new BeanWrapperImpl(pt);
		bw.setPropertyValue("name", "ptest");
		
		String ps = "peace=war\nfreedom=slavery";
		bw.setPropertyValue("properties", ps);
		
		assertTrue(pt.name.equals("ptest"));
		assertTrue(pt.props != null);
		String freedomVal = pt.props.getProperty("freedom");
		String peaceVal = pt.props.getProperty("peace");
		assertTrue(peaceVal.equals("war"));
		assertTrue(freedomVal.equals("slavery"));
	}
	
	@Test
	public void testStringArrayProperty() throws Exception {
		
	}
	
	@Test
	public void testStringArrayPropertyWithCustomStringEditor() throws Exception {
		
	}
	
	@Test
	public void testStringArrayPropertyWithStringSplitting() throws Exception {
		
	}
	
	@Test
	public void testStringArrayPropertyWithCustomStringDelimiter() throws Exception {
		
	}
	
	@Test
	public void testStringArrayPropertyWithCustomEditor() throws Exception {
		
	}
	
	@Test
	public void testIntArrayProperty() {
		
	}
	
	@Test
	public void testIntArrayPropertyWithCustomEditor() {
		
	}
	
	@Test
	public void testIntArrayPropertyWithStringSplitting() {
		
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
	
	private static class PropsTester {
		private Properties props;
		
		private String name;
		
		private String[] stringArray;
		
		private int[] intArray;

		public void setProperties(Properties props) {
			this.props = props;
		}

		public void setName(String name) {
			this.name = name;
		}

		public void setStringArray(String[] stringArray) {
			this.stringArray = stringArray;
		}

		public void setIntArray(int[] intArray) {
			this.intArray = intArray;
		}
		
	}
	
}
