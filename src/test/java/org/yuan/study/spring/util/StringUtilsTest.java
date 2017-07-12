package org.yuan.study.spring.util;

import java.util.Set;
import java.util.TreeSet;

import org.junit.Test;

public class StringUtilsTest {

	@Test
	public void testToStringArray() {
		Set<String> set = new TreeSet<String>();
		set.add("1");
		set.add("2");
		set.add("3");
		
		org.junit.Assert.assertArrayEquals(
			new String[]{"1", "2", "3"}, StringUtils.toStringArray(set));
	}
}
