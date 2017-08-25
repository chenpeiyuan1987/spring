package org.yuan.study.spring.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

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
	
	@Test
	public void testHasText() throws Exception {
		assertFalse(StringUtils.hasText("       "));
		assertFalse(StringUtils.hasText(""));
		assertFalse(StringUtils.hasText(null));
		
		assertTrue(StringUtils.hasText("t"));
	}
	
	@Test
	public void testReplace() throws Exception {
		String inString = "a6AazAaa77abaa";
		String oldPattern = "aa";
		String newPattern = "foo";
		
		// Simple replace
		String s = StringUtils.replace(inString, oldPattern, newPattern);
		assertTrue(s.equals("a6AazAfoo77abfoo"));
		
		// Non match: no change
		s = StringUtils.replace(inString, "qwoeiruqopwieurpoqwieur", newPattern);
		assertTrue(s.equals(inString));
		
		// Null new pattern: should ignore
		s = StringUtils.replace(inString, oldPattern, null);
		assertTrue(s.equals(inString));
		
		// Null old pattern: should ignore
		s = StringUtils.replace(inString, null, newPattern);
		assertTrue(s.equals(inString));
	}
	
	@Test
	public void testDelimitedListToStringArrayWithComma() {
		String[] sa = StringUtils.delimitedListToStringArray("a,b", ",");
		assertEquals(2, sa.length);
		assertEquals("a", sa[0]);
		assertEquals("b", sa[1]);
	}
	
	@Test
	public void testDelimitedListToStringArrayWithSemicolon() {
		String[] sa = StringUtils.delimitedListToStringArray("a;b", ";");
		assertEquals(2, sa.length);
		assertEquals("a", sa[0]);
		assertEquals("b", sa[1]);
	}
	
	@Test
	public void testDelimitedListToStringArrayWithEmptyString() {
		String[] sa = StringUtils.delimitedListToStringArray("a,b", "");
		assertEquals(3, sa.length);
		assertEquals("a", sa[0]);
		assertEquals(",", sa[1]);
		assertEquals("b", sa[2]);
	}
	
	@Test
	public void testDelimitedListToStringArrayWithNullDelimiter() {
		String[] sa = StringUtils.delimitedListToStringArray("a,b", null);
		assertEquals(1, sa.length);
		assertEquals("a,b", sa[0]);
	}
	
	@Test
	public void testCleanPath() {
		assertEquals("mypath/myfile", StringUtils.cleanPath("mypath/myfile"));
		assertEquals("mypath/myfile", StringUtils.cleanPath("mypath\\myfile"));
		assertEquals("mypath/myfile", StringUtils.cleanPath("mypath/../mypath/myfile"));
		assertEquals("mypath/myfile", StringUtils.cleanPath("mypath/myfile/../../mypath/myfile"));
		assertEquals("../mypath/myfile", StringUtils.cleanPath("../mypath/myfile"));
		assertEquals("../mypath/myfile", StringUtils.cleanPath("../mypath/../mypath/myfile"));
		assertEquals("../mypath/myfile", StringUtils.cleanPath("mypath/../../mypath/myfile"));
		assertEquals("../mypath/myfile", StringUtils.cleanPath("mypath/../../mypath/myfile"));
		assertEquals("/../mypath/myfile", StringUtils.cleanPath("/../mypath/myfile"));
		assertEquals("/mypath/myfile", StringUtils.cleanPath("/a/:b/../../mypath/myfile"));
		assertEquals("file:///c:/path/to/the%20file.txt", StringUtils.cleanPath("file:///c:/some/../path/to/the%20file.txt"));
	}
	
	@Test
	public void testGetFilename() {
		assertEquals(null, StringUtils.getFilename(null));
		assertEquals("", StringUtils.getFilename(""));
		assertEquals("myfile", StringUtils.getFilename("myfile"));
		assertEquals("myfile", StringUtils.getFilename("mypath/myfile"));
		assertEquals("myfile.txt", StringUtils.getFilename("myfile.txt"));
		assertEquals("myfile.txt", StringUtils.getFilename("mypath/myfile.txt"));
	}
	
	@Test
	public void testTokenizeToStringArray() {
		String[] sa = StringUtils.tokenizeToStringArray("a,b , ,c", ",");
		assertEquals(3, sa.length);
		assertEquals("a", sa[0]);
		assertEquals("b", sa[1]);
		assertEquals("c", sa[2]);
	}
	
	@Test
	public void testTokenizeToStringArrayWithNotIgnoreEmptyTokens() {
		String[] sa = StringUtils.tokenizeToStringArray("a,b , ,c", ",", true, false);
		assertEquals(4, sa.length);
		assertEquals("a", sa[0]);
		assertEquals("b", sa[1]);
		assertEquals("", sa[2]);
		assertEquals("c", sa[3]);
	}
	
	@Test
	public void testTokenizeToStringArrayWithNotTrimTokens() {
		String[] sa = StringUtils.tokenizeToStringArray("a,b , ,c", ",", false, true);
		assertEquals(4, sa.length);
		assertEquals("a", sa[0]);
		assertEquals("b ", sa[1]);
		assertEquals(" ", sa[2]);
		assertEquals("c", sa[3]);
	}
	
	@Test
	public void testCountOccurrencesOf() {
		assertEquals(0, StringUtils.countOccurrencesOf(null, null));
		assertEquals(0, StringUtils.countOccurrencesOf("s", null));
		assertEquals(0, StringUtils.countOccurrencesOf(null, "s"));
		assertEquals(0, StringUtils.countOccurrencesOf("s", ""));
		assertEquals(0, StringUtils.countOccurrencesOf("", "s"));
		
		String s = "abcdaac";
		assertEquals(0, StringUtils.countOccurrencesOf(s, "e"));
		assertEquals(3, StringUtils.countOccurrencesOf(s, "a"));
		assertEquals(2, StringUtils.countOccurrencesOf(s, "c"));
		assertEquals(1, StringUtils.countOccurrencesOf(s, "ab"));
	}
}
