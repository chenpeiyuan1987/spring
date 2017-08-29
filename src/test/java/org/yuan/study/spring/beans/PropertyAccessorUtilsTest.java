package org.yuan.study.spring.beans;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

public final class PropertyAccessorUtilsTest {

	@Test
	public void testCanonicalPropertyName() {
		assertEquals("map", PropertyAccessorUtils.canonicalPropertyName("map"));
		assertEquals("map[key1]", PropertyAccessorUtils.canonicalPropertyName("map[key1]"));
		assertEquals("map[key1]", PropertyAccessorUtils.canonicalPropertyName("map['key1']"));
		assertEquals("map[key1]", PropertyAccessorUtils.canonicalPropertyName("map[\"key1\"]"));
		assertEquals("map[key1][key2]", PropertyAccessorUtils.canonicalPropertyName("map[key1][key2]"));
		assertEquals("map[key1][key2]", PropertyAccessorUtils.canonicalPropertyName("map['key1'][\"key2\"]"));
		assertEquals("map[key1].name", PropertyAccessorUtils.canonicalPropertyName("map[key1].name"));
		assertEquals("map[key1].name", PropertyAccessorUtils.canonicalPropertyName("map['key1'].name"));
		assertEquals("map[key1].name", PropertyAccessorUtils.canonicalPropertyName("map[\"key1\"].name"));
	}
	
	@Test
	public void testCanonicalPropertyNames() {
		String[] original = {
			"map", "map[key1]", "map['key1']", "map[\"key1\"]", 
			"map[key1][key2]", "map['key1'][\"key2\"]", "map[key1].name",
			"map['key1'].name", "map[\"key1\"].name"
		};
		String[] canonical = {
			"map", "map[key1]", "map[key1]", "map[key1]", 
			"map[key1][key2]", "map[key1][key2]", "map[key1].name",
			"map[key1].name", "map[key1].name"
		};
		
		assertArrayEquals(canonical, PropertyAccessorUtils.canonicalPropertyNames(original));
	}
}
