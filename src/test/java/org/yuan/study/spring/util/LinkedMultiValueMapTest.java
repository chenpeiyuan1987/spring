package org.yuan.study.spring.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

public class LinkedMultiValueMapTest {

	private LinkedMultiValueMap<String, String> map;
	
	@Before
	public void setUp() {
		map = new LinkedMultiValueMap<String, String>();
	}
	
	@Test
	public void add() {
		map.add("key", "value1");
		map.add("key", "value2");
		assertEquals(1, map.size());
		
		List<String> expect = new ArrayList<String>(2);
		expect.add("value1");
		expect.add("value2");
		assertEquals(expect, map.get("key"));
	}
	
	@Test
	public void getFirst() {
		List<String> values = new ArrayList<String>(2);
		values.add("value1");
		values.add("value2");
		map.put("key", values);
		assertEquals("value1", map.getFirst("key"));
		assertNull(map.getFirst("other"));
	}
	
	@Test
	public void set() {
		map.set("key", "value1");
		map.set("key", "value2");
		assertEquals(1, map.size());
		assertEquals(Collections.singletonList("value2"), map.get("key"));
	}
	
	@Test
	public void equals() {
		map.set("key1", "value1");
		assertEquals(map, map);
		
		MultiValueMap<String,String> o1 = new LinkedMultiValueMap<String, String>();
		o1.set("key1", "value1");
		assertEquals(map, o1);
		assertEquals(o1, map);
		
		Map<String, List<String>> o2 = new HashMap<String, List<String>>();
		o2.put("key1", Collections.singletonList("value1"));
		assertEquals(map, o2);
		assertEquals(o2, map);
	}
}
