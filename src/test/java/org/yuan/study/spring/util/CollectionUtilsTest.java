package org.yuan.study.spring.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.junit.Test;

@SuppressWarnings({"rawtypes", "unchecked"})
public class CollectionUtilsTest {

	@Test
	public void testIsEmpty() {
		assertTrue(CollectionUtils.isEmpty((Set) null));
		assertTrue(CollectionUtils.isEmpty((Map) null));
		assertTrue(CollectionUtils.isEmpty(new HashMap()));
		assertTrue(CollectionUtils.isEmpty(new HashSet()));
		
		List list = new LinkedList();
		list.add(new Object());
		assertFalse(CollectionUtils.isEmpty(list));
		
		Map map = new HashMap();
		map.put("foo", "bar");
		assertFalse(CollectionUtils.isEmpty(map));
	}
	
	@Test
	public void testMergeArrayIntoCollection() {
		Object[] array = new Object[] {"value1", "value2"};
		List list = new LinkedList();
		list.add("value3");
		
		CollectionUtils.mergeArrayIntoCollection(array, list);
		assertEquals("value3", list.get(0));
		assertEquals("value1", list.get(1));
		assertEquals("value2", list.get(2));
	}
	
	@Test
	public void testMergePrimitiveArrayIntoCollection() {
		int[] array = new int[] {1, 2};
		List list = new LinkedList();
		list.add(new Integer(3));
		
		CollectionUtils.mergeArrayIntoCollection(array, list);
		assertEquals(new Integer(3), list.get(0));
		assertEquals(new Integer(1), list.get(1));
		assertEquals(new Integer(2), list.get(2));
	}
	
	@Test
	public void testMergePropertiesIntoMap() {
		Properties defaults = new Properties();
		defaults.setProperty("prop1", "value1");
		Properties props = new Properties(defaults);
		props.setProperty("prop2", "value2");
		props.put("prop3", new Integer(3));
		
		Map map = new HashMap();
		map.put("prop4", "value4");
		
		CollectionUtils.mergePropertiesIntoMap(props, map);
		assertEquals("value1", map.get("prop1"));
		assertEquals("value2", map.get("prop2"));
		assertEquals(new Integer(3), map.get("prop3"));
		assertEquals("value4", map.get("prop4"));
	}
	
	@Test
	public void testContains() {
		assertFalse(CollectionUtils.contains((Iterator) null, "myElement"));
		assertFalse(CollectionUtils.contains((Enumeration) null, "myElement"));
		assertFalse(CollectionUtils.contains(new LinkedList().iterator(), "myElement"));
		assertFalse(CollectionUtils.contains(new Hashtable().keys(), "myElement"));
		
		List list = new LinkedList();
		list.add("myElement");
		assertTrue(CollectionUtils.contains(list.iterator(), "myElement"));
		
		Hashtable ht = new Hashtable();
		ht.put("myElement", "myValue");
		assertTrue(CollectionUtils.contains(ht.keys(), "myElement"));
	}
	
	@Test
	public void testContainsAny() {
		List source = new ArrayList();
		source.add("abc");
		source.add("def");
		source.add("ghi");
		
		List candidates = new ArrayList();
		candidates.add("xyz");
		candidates.add("def");
		candidates.add("abc");
		
		assertTrue(CollectionUtils.containsAny(source, candidates));
		candidates.remove("def");
		assertTrue(CollectionUtils.containsAny(source, candidates));
		candidates.remove("abc");
		assertFalse(CollectionUtils.containsAny(source, candidates));
	}
	
	@Test
	public void testContainsInstanceWithNullCollection() {
		assertFalse(CollectionUtils.containsInstance(null, this));
	}
	
	@Test
	public void testContainsInstanceWithInstancesThatAreEqualButDistinct() {
		List list = new ArrayList();
		list.add(new Instance("fiona"));
		assertFalse(CollectionUtils.containsInstance(list, new Instance("fiona")));
	}
	
	@Test
	public void testContainsInstanceWithSameInstance() {
		List list = new ArrayList();
		list.add(new Instance("apple"));
		Instance instance = new Instance("fiona");
		list.add(instance);
		assertTrue(CollectionUtils.containsInstance(list, instance));
	}
	
	@Test
	public void testContainsInstanceWithNullInstance() {
		List list = new ArrayList();
		list.add(new Instance("apple"));
		list.add(new Instance("fiona"));
		assertFalse(CollectionUtils.containsInstance(list, null));
	}
	
	@Test
	public void testFindFirstMatch() {
		List source = new ArrayList();
		source.add("abc");
		source.add("def");
		source.add("ghi");
		
		List candidates = new ArrayList();
		candidates.add("xyz");
		candidates.add("def");
		candidates.add("abc");
		
		assertEquals("def", CollectionUtils.findFirstMatch(source, candidates));
	}
	
	@Test
	public void testHasUniqueObject() {
		List list = new LinkedList();
		list.add("myElement");
		list.add("myOtherElement");
		assertFalse(CollectionUtils.hasUniqueObject(list));
		
		list = new LinkedList();
		list.add("myElement");
		assertTrue(CollectionUtils.hasUniqueObject(list));
		
		list = new LinkedList();
		list.add("myElement");
		list.add(null);
		assertFalse(CollectionUtils.hasUniqueObject(list));
		
		list = new LinkedList();
		list.add(null);
		list.add("myElement");
		assertFalse(CollectionUtils.hasUniqueObject(list));
		
		list = new LinkedList();
		list.add(null);
		list.add(null);
		assertTrue(CollectionUtils.hasUniqueObject(list));
		
		list = new LinkedList();
		list.add(null);
		assertTrue(CollectionUtils.hasUniqueObject(list));
		
		list = new LinkedList();
		assertFalse(CollectionUtils.hasUniqueObject(list));
	}
	
	//----------------------------------------------------------------------
	//
	//----------------------------------------------------------------------
	
	private static final class Instance {
		private final String name;

		public Instance(String name) {
			this.name = name;
		}

		@Override
		public int hashCode() {
			return name.hashCode();
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			Instance instance = (Instance) obj;
			return name.equals(instance.name);
		}
	}
}
