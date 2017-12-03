package org.yuan.study.spring.beans;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.beans.PropertyEditorSupport;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import org.junit.Test;
import org.yuan.study.spring.beans.factory.annotation.Autowire;
import org.yuan.study.spring.beans.propertyeditors.StringArrayPropertyEditor;
import org.yuan.study.spring.beans.propertyeditors.StringTrimmerEditor;
import org.yuan.study.spring.util.StringUtils;

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
		PropsTester pt = new PropsTester();
		BeanWrapper bw = new BeanWrapperImpl(pt);
		
		bw.setPropertyValue("stringArray", new String[] {"foo", "fi", "fi", "fum"});
		assertEquals(4, pt.stringArray.length);
		assertEquals("foo", pt.stringArray[0]);
		assertEquals("fi", pt.stringArray[1]);
		assertEquals("fi", pt.stringArray[2]);
		assertEquals("fum", pt.stringArray[3]);
		
		List<String> list = new ArrayList<String>();
		list.add("foo");
		list.add("fi");
		list.add("fi");
		list.add("fum");
		bw.setPropertyValue("stringArray", list);
		assertEquals(4, pt.stringArray.length);
		assertEquals("foo", pt.stringArray[0]);
		assertEquals("fi", pt.stringArray[1]);
		assertEquals("fi", pt.stringArray[2]);
		assertEquals("fum", pt.stringArray[3]);
		
		Set<String> set = new HashSet<String>();
		set.add("foo");
		set.add("fi");
		set.add("fum");
		bw.setPropertyValue("stringArray", set);
		assertEquals(3, pt.stringArray.length);
		List<String> result = Arrays.asList(pt.stringArray);
		assertTrue(result.contains("foo"));
		assertTrue(result.contains("fi"));
		assertTrue(result.contains("fum"));
		
		bw.setPropertyValue("stringArray", "one");
		assertEquals(1, pt.stringArray.length);
		assertEquals("one", pt.stringArray[0]);
		
		bw.setPropertyValue("stringArray", null);
		assertNull(pt.stringArray);
	}
	
	@Test
	public void testStringArrayPropertyWithCustomStringEditor() throws Exception {
		PropsTester pt = new PropsTester();
		BeanWrapper bw = new BeanWrapperImpl(pt);
		bw.registerCustomEditor(String.class, "stringArray", new PropertyEditorSupport() {
			@Override
			public void setAsText(String text) throws IllegalArgumentException {
				setValue(text.substring(1));
			}
		});
		
		bw.setPropertyValue("stringArray", new String[] {"0foo", "0fi", "0fi", "0fum"});
		assertEquals(4, pt.stringArray.length);
		assertEquals("foo", pt.stringArray[0]);
		assertEquals("fi", pt.stringArray[1]);
		assertEquals("fi", pt.stringArray[2]);
		assertEquals("fum", pt.stringArray[3]);
		
		List<String> list = new ArrayList<String>();
		list.add("0foo");
		list.add("0fi");
		list.add("0fi");
		list.add("0fum");
		bw.setPropertyValue("stringArray", list);
		assertEquals(4, pt.stringArray.length);
		assertEquals("foo", pt.stringArray[0]);
		assertEquals("fi", pt.stringArray[1]);
		assertEquals("fi", pt.stringArray[2]);
		assertEquals("fum", pt.stringArray[3]);
		
		Set<String> set = new HashSet<String>();
		set.add("0foo");
		set.add("0fi");
		set.add("0fum");
		bw.setPropertyValue("stringArray", set);
		assertEquals(3, pt.stringArray.length);
		List<String> result = Arrays.asList(pt.stringArray);
		assertTrue(result.contains("foo"));
		assertTrue(result.contains("fi"));
		assertTrue(result.contains("fum"));
		
		bw.setPropertyValue("stringArray", "0one");
		assertEquals(1, pt.stringArray.length);
		assertEquals("one", pt.stringArray[0]);
	}
	
	@Test
	public void testStringArrayPropertyWithStringSplitting() throws Exception {
		PropsTester pt = new PropsTester();
		BeanWrapperImpl bw = new BeanWrapperImpl(pt);
		bw.useConfigValueEditors();
		bw.setPropertyValue("stringArray", "a1,b2");
		assertEquals(2, pt.stringArray.length);
		assertEquals("a1", pt.stringArray[0]);
		assertEquals("b2", pt.stringArray[1]);
	}
	
	@Test
	public void testStringArrayPropertyWithCustomStringDelimiter() throws Exception {
		PropsTester pt = new PropsTester();
		BeanWrapperImpl bw = new BeanWrapperImpl(pt);
		bw.registerCustomEditor(String[].class, "stringArray", new StringArrayPropertyEditor("-"));
		bw.setPropertyValue("stringArray", "a1-b2");
		assertEquals(2, pt.stringArray.length);
		assertEquals("a1", pt.stringArray[0]);
		assertEquals("b2", pt.stringArray[1]);
	}
	
	@Test
	public void testStringPropertyWithCustomEditor() throws Exception {
		TestBean tb = new TestBean();
		BeanWrapper bw = new BeanWrapperImpl(tb);
		bw.registerCustomEditor(String.class, "name", new PropertyEditorSupport() {
			@Override
			public void setValue(Object value) {
				if (value instanceof String[]) {
					setValue(StringUtils.arrayToDelimitedString((String[])value, "-"));
				} 
				else {
					super.setValue(value != null ? value : "");
				}
			}
		});
		bw.setPropertyValue("name", new String[]{});
		assertEquals("", tb.getName());
		bw.setPropertyValue("name", new String[]{"a1", "b2"});
		assertEquals("a1-b2", tb.getName());
		bw.setPropertyValue("name", null);
		assertEquals("", tb.getName());
	}
	
	@Test
	public void testIntArrayProperty() {
		PropsTester pt = new PropsTester();
		BeanWrapper bw = new BeanWrapperImpl(pt);
		
		bw.setPropertyValue("intArray", new int[] {4, 5, 2, 3});
		assertEquals(4, pt.intArray.length);
		assertEquals(4, pt.intArray[0]);
		assertEquals(5, pt.intArray[1]);
		assertEquals(2, pt.intArray[2]);
		assertEquals(3, pt.intArray[3]);
		
		List<Object> list = new ArrayList<Object>();
		list.add(new Integer(4));
		list.add("5");
		list.add(new Integer(2));
		list.add("3");
		bw.setPropertyValue("intArray", list);
		assertEquals(4, pt.intArray.length);
		assertEquals(4, pt.intArray[0]);
		assertEquals(5, pt.intArray[1]);
		assertEquals(2, pt.intArray[2]);
		assertEquals(3, pt.intArray[3]);
		
		Set<Object> set = new HashSet<Object>();
		set.add("4");
		set.add(new Integer(5));
		set.add("3");
		bw.setPropertyValue("intArray", set);
		assertEquals(3, pt.intArray.length);
		List<Integer> result = new ArrayList<Integer>();
		result.add(new Integer(pt.intArray[0]));
		result.add(new Integer(pt.intArray[1]));
		result.add(new Integer(pt.intArray[2]));
		assertTrue(result.contains(new Integer(4)));
		assertTrue(result.contains(new Integer(5)));
		assertTrue(result.contains(new Integer(3)));
		
		bw.setPropertyValue("intArray", new Integer[] {new Integer(1)});
		assertEquals(1, pt.intArray.length);
		assertEquals(1, pt.intArray[0]);
		
		bw.setPropertyValue("intArray", new Integer(1));
		assertEquals(1, pt.intArray.length);
		assertEquals(1, pt.intArray[0]);
		
		bw.setPropertyValue("intArray", new String[] {"1"});
		assertEquals(1, pt.intArray.length);
		assertEquals(1, pt.intArray[0]);
		
		bw.setPropertyValue("intArray", "1");
		assertEquals(1, pt.intArray.length);
		assertEquals(1, pt.intArray[0]);
	}
	
	@Test
	public void testIntArrayPropertyWithCustomEditor() {
		PropsTester pt = new PropsTester();
		BeanWrapper bw = new BeanWrapperImpl(pt);
		bw.registerCustomEditor(int.class, new PropertyEditorSupport() {
			@Override
			public void setAsText(String text) throws IllegalArgumentException {
				setValue(new Integer(Integer.parseInt(text) + 1));
			}
		});
		
		bw.setPropertyValue("intArray", new int[] {4, 5, 2, 3});
		assertEquals(4, pt.intArray.length);
		assertEquals(4, pt.intArray[0]);
		assertEquals(5, pt.intArray[1]);
		assertEquals(2, pt.intArray[2]);
		assertEquals(3, pt.intArray[3]);
		
		bw.setPropertyValue("intArray", new String[] {"4", "5", "2", "3"});
		assertEquals(4, pt.intArray.length);
		assertEquals(5, pt.intArray[0]);
		assertEquals(6, pt.intArray[1]);
		assertEquals(3, pt.intArray[2]);
		assertEquals(4, pt.intArray[3]);
		
		bw.setPropertyValue("intArray", new Integer(1));
		assertEquals(1, pt.intArray.length);
		assertEquals(1, pt.intArray[0]);
		
		bw.setPropertyValue("intArray", new String[] {"1"});
		assertEquals(1, pt.intArray.length);
		assertEquals(2, pt.intArray[0]);
		
		bw.setPropertyValue("intArray", "1");
		assertEquals(1, pt.intArray.length);
		assertEquals(2, pt.intArray[0]);
	}
	
	@Test
	public void testIntArrayPropertyWithStringSplitting() {
		PropsTester pt = new PropsTester();
		BeanWrapperImpl bw = new BeanWrapperImpl(pt);
		bw.useConfigValueEditors();
		bw.setPropertyValue("intArray", "4,5");
		assertEquals(2, pt.intArray.length);
		assertEquals(4, pt.intArray[0]);
		assertEquals(5, pt.intArray[1]);
	}
	
	@Test
	public void testIndividualAllValid() {
		
	}
	
	@Test
	public void test2Invalid() {
		
	}
	
	@Test
	public void testPossibleMatches() {
		
	}
	
	@Test
	public void testTypeMismatch() {
		
	}
	
	@Test
	public void testEmptyValueForPrimitiveProperty() {
		
	}
	
	@Test
	public void testSetPropertyValuesIgnoresInvalidNestedOnRequest() {
		
	}
	
	@Test
	public void testGetNestedProperty() {
		
	}
	
	@Test
	public void testGetNestedPropertyNullValue() {
		
	}
	
	@Test
	public void testSetNestedProperty() {
		
	}
	
	@Test
	public void testSetNestedPropertyNullValue() {
		
	}
	
	@Test
	public void testSetNestedPropertyPolymorphic() {
		
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testNullObject() {
		new BeanWrapperImpl((Object) null);
	}
	
	@Test
	public void testNestedProperties() {
		
	}
	
	@Test
	public void testIndexedProperties() {
		
	}
	
	@Test
	public void testIndexedPropertiesWithDirectAccess() {
		
	}
	
	@Test
	public void testMapAccessWithTypeConversion() {
		
	}
	
	@Test
	public void testMapAccessWithUnmodifiableMap() {
		
	}
	
	@Test
	public void testMapAccessWithCustomUnmodifiableMap() {
		
	}
	
	@Test
	public void testRawMapAccessWithNoEditorRegistered() {
		
	}
	
	@Test
	public void testTypedMapReadOnlyMap() {
		
	}
	
	@Test
	public void testPrimitiveArray() {
		
	}
	
	@Test
	public void testLargeMatchingPrimitiveArray() {
		
	}
	
	@Test
	public void testLargeMatchingPrimitiveArrayWithSpecificEditor() {
		
	}
	
	@Test
	public void testLargeMatchingPrimitiveArrayWithIndexSpecificEditor() {
		
	}
	
	@Test
	public void testPropertiesInProtectedBaseBean() {
		
	}
	
	@Test
	public void testErrorMessageOfNestedProperty() {
		
	}
	
	@Test
	public void testMatchingCollections() {
		
	}
	
	@Test
	public void testNonMatchingCollections() {
		
	}
	
	@Test
	public void testCollectionsWithArrayValues() {
		
	}
	
	@Test
	public void testCollectionsWithIntArrayValues() {
		
	}
	
	@Test
	public void testCollectionsWithIntegerValues() {
		
	}
	
	@Test
	public void testCollectionsWithStringValues() {
		
	}
	
	@Test
	public void testCollectionsWithStringValuesAndCustomEditor() {
		
	}
	
	@Test
	public void testMatchingMaps() {
		IndexedTestBean tb = new IndexedTestBean();
		BeanWrapper bw = new BeanWrapperImpl(tb);
		Map<String, String> map = new HashMap<String, String>();
		map.put("key", "value");
		bw.setPropertyValue("map", map);
		SortedMap<?, ?> sortedMap = new TreeMap<Object, Object>();
		map.put("sortedKey", "sortedValue");
		bw.setPropertyValue("sortedMap", sortedMap);
		assertSame(map, tb.getMap());
		assertSame(sortedMap, tb.getSortedMap());
	}
	
	@Test
	public void testNonMatchingMaps() {
		IndexedTestBean tb = new IndexedTestBean();
		BeanWrapper bw = new BeanWrapperImpl(tb);
		Map<String, String> map = new HashMap<String, String>();
		map.put("key", "value");
		bw.setPropertyValue("map", map);
		SortedMap<String, String> sortedMap = new TreeMap<String, String>();
		sortedMap.put("sortedKey", "sortedValue");
		bw.setPropertyValue("sortedMap", sortedMap);
		assertEquals(1, tb.getMap().size());
		assertEquals("value", tb.getMap().get("key"));
		assertEquals(1, tb.getSortedMap().size());
		assertEquals("sortedValue", tb.getSortedMap().get("sortedKey"));
	}
	
	@Test
	public void testSetNumberProperties() {
		NumberPropertyBean bean = new NumberPropertyBean();
		BeanWrapper bw = new BeanWrapperImpl(bean);
		
		String byteValue = " " + Byte.MAX_VALUE + " ";
		String shortValue = " " + Short.MAX_VALUE + " ";
		String intValue = " " + Integer.MAX_VALUE + " ";
		String longValue = " " + Long.MAX_VALUE + " ";
		String floatValue = " " + Float.MAX_VALUE + " ";
		String doubleValue = " " + Double.MAX_VALUE + " ";
		
		bw.setPropertyValue("myPrimitiveByte", byteValue);
		bw.setPropertyValue("myByte", byteValue);
		
		assertEquals(Byte.MAX_VALUE, bean.getMyPrimitiveByte());
		assertEquals(Byte.MAX_VALUE, bean.getMyByte().byteValue());
		
		bw.setPropertyValue("myPrimitiveShort", shortValue);
		bw.setPropertyValue("myShort", shortValue);
		
		assertEquals(Short.MAX_VALUE, bean.getMyPrimitiveShort());
		assertEquals(Short.MAX_VALUE, bean.getMyShort().shortValue());
		
		bw.setPropertyValue("myPrimitiveInt", intValue);
		bw.setPropertyValue("myInteger", intValue);
		
		assertEquals(Integer.MAX_VALUE, bean.getMyPrimitiveInt());
		assertEquals(Integer.MAX_VALUE, bean.getMyInteger().intValue());
		
		bw.setPropertyValue("myPrimitiveLong", longValue);
		bw.setPropertyValue("myLong", longValue);
		
		assertEquals(Long.MAX_VALUE, bean.getMyPrimitiveLong());
		assertEquals(Long.MAX_VALUE, bean.getMyLong().longValue());
		
		bw.setPropertyValue("myPrimitiveFloat", floatValue);
		bw.setPropertyValue("myFloat", floatValue);
		
		assertEquals(Float.MAX_VALUE, bean.getMyPrimitiveFloat(), 0);
		assertEquals(Float.MAX_VALUE, bean.getMyFloat().floatValue(), 0);
		
		bw.setPropertyValue("myPrimitiveDouble", doubleValue);
		bw.setPropertyValue("myDouble", doubleValue);
		
		assertEquals(Double.MAX_VALUE, bean.getMyPrimitiveDouble(), 0);
		assertEquals(Double.MAX_VALUE, bean.getMyDouble().doubleValue(), 0);
	}
	
	@Test
	public void testAlternativesForTypo() {
		IntelliBean ib = new IntelliBean();
		BeanWrapper bw = new BeanWrapperImpl(ib);
		try {
			bw.setPropertyValue("names", "Alef");
		} 
		catch (NotWritablePropertyException ex) {
			assertTrue(ex.getPossibleMatches() != null);
			assertEquals(1, ex.getPossibleMatches().length);
		}
	}
	
	@Test
	public void testAlternativesForTypos() {
		IntelliBean ib = new IntelliBean();
		BeanWrapper bw = new BeanWrapperImpl(ib);
		try {
			bw.setPropertyValue("mystring", "Arjen");
		} 
		catch (NotWritablePropertyException ex) {
			assertTrue(ex.getPossibleMatches() != null);
			assertEquals(3, ex.getPossibleMatches().length);
		}
	}
	
	@Test
	public void testGenericEnum() {
		EnumConsumer consumer = new EnumConsumer();
		BeanWrapper bw = new BeanWrapperImpl(consumer);
		bw.setPropertyValue("enumValue", TestEnum.class.getName() + ".TEST_VALUE");
		assertEquals(TestEnum.TEST_VALUE, consumer.getEnumValue());
	}
	
	@Test
	public void testWildcardedGenericEnum() {
		WildcardEnumConsumer consumer = new WildcardEnumConsumer();
		BeanWrapper bw = new BeanWrapperImpl(consumer);
		bw.setPropertyValue("enumValue", TestEnum.class.getName() + ".TEST_VALUE");
		assertEquals(TestEnum.TEST_VALUE, consumer.getEnumValue());
	}
	
	//------------------------------------------------------
	// Private static class for test use
	//------------------------------------------------------
	
	private static class DifferentTestBean extends TestBean {
		
	}
	
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
	
	private static class ThrowsException {
		
		public void doSomething(Throwable t) throws Throwable {
			throw t;
		}
	}
	
	private static class PrimitiveArrayBean {
		
		private int[] array;

		public int[] getArray() {
			return array;
		}

		public void setArray(int[] array) {
			this.array = array;
		}
		
	}
	
	private static class NumberPropertyBean {
		
		private byte myPrimitiveByte;
		private Byte myByte;
		
		private short myPrimitiveShort;
		private Short myShort;
		
		private int myPrimitiveInt;
		private Integer myInteger;
		
		private long myPrimitiveLong;
		private Long myLong;
		
		private float myPrimitiveFloat;
		private Float myFloat;
		
		private double myPrimitiveDouble;
		private Double myDouble;
		
		
		public byte getMyPrimitiveByte() {
			return myPrimitiveByte;
		}
		public void setMyPrimitiveByte(byte myPrimitiveByte) {
			this.myPrimitiveByte = myPrimitiveByte;
		}
		public Byte getMyByte() {
			return myByte;
		}
		public void setMyByte(Byte myByte) {
			this.myByte = myByte;
		}
		public short getMyPrimitiveShort() {
			return myPrimitiveShort;
		}
		public void setMyPrimitiveShort(short myPrimitiveShort) {
			this.myPrimitiveShort = myPrimitiveShort;
		}
		public Short getMyShort() {
			return myShort;
		}
		public void setMyShort(Short myShort) {
			this.myShort = myShort;
		}
		public int getMyPrimitiveInt() {
			return myPrimitiveInt;
		}
		public void setMyPrimitiveInt(int myPrimitiveInt) {
			this.myPrimitiveInt = myPrimitiveInt;
		}
		public Integer getMyInteger() {
			return myInteger;
		}
		public void setMyInteger(Integer myInteger) {
			this.myInteger = myInteger;
		}
		public long getMyPrimitiveLong() {
			return myPrimitiveLong;
		}
		public void setMyPrimitiveLong(long myPrimitiveLong) {
			this.myPrimitiveLong = myPrimitiveLong;
		}
		public Long getMyLong() {
			return myLong;
		}
		public void setMyLong(Long myLong) {
			this.myLong = myLong;
		}
		public float getMyPrimitiveFloat() {
			return myPrimitiveFloat;
		}
		public void setMyPrimitiveFloat(float myPrimitiveFloat) {
			this.myPrimitiveFloat = myPrimitiveFloat;
		}
		public Float getMyFloat() {
			return myFloat;
		}
		public void setMyFloat(Float myFloat) {
			this.myFloat = myFloat;
		}
		public double getMyPrimitiveDouble() {
			return myPrimitiveDouble;
		}
		public void setMyPrimitiveDouble(double myPrimitiveDouble) {
			this.myPrimitiveDouble = myPrimitiveDouble;
		}
		public Double getMyDouble() {
			return myDouble;
		}
		public void setMyDouble(Double myDouble) {
			this.myDouble = myDouble;
		}
	}
	
	private static class IntelliBean {
		
		public void setName(String name) {}
		
		public void setMyString(String string) {}
		
		public void setMyStrings(String string) {}
		
		public void setMyStriNg(String string) {}
		
		public void setMyStringss(String string) {}
	}
	
	private static class Employee extends TestBean {}
	
	public static class ReadOnlyMap<K, V> extends HashMap<K, V> {}
	
	public static class TypedReadOnlyMap extends ReadOnlyMap<String, TestBean> {}
	
	public static class TypedReadOnlyMapClient {}
	
	public static class EnumConsumer {
		
		private Enum<TestEnum> enumValue;

		public Enum<TestEnum> getEnumValue() {
			return enumValue;
		}

		public void setEnumValue(Enum<TestEnum> enumValue) {
			this.enumValue = enumValue;
		}
		
	}
	
	public static class WildcardEnumConsumer {
		
		private Enum<?> enumValue;

		public Enum<?> getEnumValue() {
			return enumValue;
		}

		public void setEnumValue(Enum<?> enumValue) {
			this.enumValue = enumValue;
		}
		
	}
	
	public enum TestEnum {
		
		TEST_VALUE;
	}
}
