package org.yuan.study.spring.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Set;
import java.util.TreeSet;

import org.junit.Test;

public class StringUtilsTest {

	@Test
	public void testHasText() throws Exception {
		assertFalse(StringUtils.hasText("       "));
		assertFalse(StringUtils.hasText(""));
		assertFalse(StringUtils.hasText(null));
		
		assertTrue(StringUtils.hasText("t"));
	}
	
	@Test
	public void testContainsWhitespace() {
		assertFalse(StringUtils.containsWhitespace(null));
		assertFalse(StringUtils.containsWhitespace(""));
		assertFalse(StringUtils.containsWhitespace("a"));
		assertFalse(StringUtils.containsWhitespace("abc"));
		
		assertTrue(StringUtils.containsWhitespace(" "));
		assertTrue(StringUtils.containsWhitespace(" a"));
		assertTrue(StringUtils.containsWhitespace("abc "));
		assertTrue(StringUtils.containsWhitespace("a b"));
		assertTrue(StringUtils.containsWhitespace("a  b"));
	}
	
	@Test
	public void testTrimWhitespace() {
		assertEquals(null, StringUtils.trimWhitespace(null));
		assertEquals("", StringUtils.trimWhitespace(""));
		assertEquals("", StringUtils.trimWhitespace(" "));
		assertEquals("", StringUtils.trimWhitespace("\t"));
		assertEquals("a", StringUtils.trimWhitespace(" a"));
		assertEquals("a", StringUtils.trimWhitespace("a "));
		assertEquals("a", StringUtils.trimWhitespace(" a "));
		assertEquals("a b", StringUtils.trimWhitespace(" a b "));
		assertEquals("a b  c", StringUtils.trimWhitespace(" a b  c "));
	}
	
	@Test
	public void testTrimAllWhitespace() {
		assertEquals("", StringUtils.trimAllWhitespace(""));
		assertEquals("", StringUtils.trimAllWhitespace(" "));
		assertEquals("", StringUtils.trimAllWhitespace("\t"));
		assertEquals("a", StringUtils.trimAllWhitespace(" a"));
		assertEquals("a", StringUtils.trimAllWhitespace("a "));
		assertEquals("a", StringUtils.trimAllWhitespace(" a "));
		assertEquals("ab", StringUtils.trimAllWhitespace(" a b "));
		assertEquals("abc", StringUtils.trimAllWhitespace(" a b  c "));
	}
	
	@Test
	public void testTrimLeadingWhitespace() {
		assertEquals(null, StringUtils.trimLeadingWhitespace(null));
		assertEquals("", StringUtils.trimLeadingWhitespace(""));
		assertEquals("", StringUtils.trimLeadingWhitespace(" "));
		assertEquals("", StringUtils.trimLeadingWhitespace("\t"));
		assertEquals("a", StringUtils.trimLeadingWhitespace(" a"));
		assertEquals("a ", StringUtils.trimLeadingWhitespace("a "));
		assertEquals("a ", StringUtils.trimLeadingWhitespace(" a "));
		assertEquals("a b ", StringUtils.trimLeadingWhitespace(" a b "));
		assertEquals("a b  c ", StringUtils.trimLeadingWhitespace(" a b  c "));
	}
	
	@Test
	public void testTrimTrailingWhitespace() {
		assertEquals(null, StringUtils.trimTrailingWhitespace(null));
		assertEquals("", StringUtils.trimTrailingWhitespace(""));
		assertEquals("", StringUtils.trimTrailingWhitespace(" "));
		assertEquals("", StringUtils.trimTrailingWhitespace("\t"));
		assertEquals("a", StringUtils.trimTrailingWhitespace("a "));
		assertEquals(" a", StringUtils.trimTrailingWhitespace(" a"));
		assertEquals(" a", StringUtils.trimTrailingWhitespace(" a "));
		assertEquals(" a b", StringUtils.trimTrailingWhitespace(" a b "));
		assertEquals(" a b  c", StringUtils.trimTrailingWhitespace(" a b  c "));
	}
	
	@Test
	public void testTrimLeadingCharacter() {
		assertEquals(null, StringUtils.trimLeadingCharacter(null, ' '));
		assertEquals("", StringUtils.trimLeadingCharacter("", ' '));
		assertEquals("", StringUtils.trimLeadingCharacter(" ", ' '));
		assertEquals("\t", StringUtils.trimLeadingCharacter("\t", ' '));
		assertEquals("a", StringUtils.trimLeadingCharacter(" a", ' '));
		assertEquals("a ", StringUtils.trimLeadingCharacter("a ", ' '));
		assertEquals("a ", StringUtils.trimLeadingCharacter(" a ", ' '));
		assertEquals("a b ", StringUtils.trimLeadingCharacter(" a b ", ' '));
		assertEquals("a b  c ", StringUtils.trimLeadingCharacter(" a b  c ", ' '));
	}
	
	@Test
	public void testTrimTrailingCharacter() {
		assertEquals(null, StringUtils.trimTrailingCharacter(null, ' '));
		assertEquals("", StringUtils.trimTrailingCharacter("", ' '));
		assertEquals("", StringUtils.trimTrailingCharacter(" ", ' '));
		assertEquals("\t", StringUtils.trimTrailingCharacter("\t", ' '));
		assertEquals("a", StringUtils.trimTrailingCharacter("a ", ' '));
		assertEquals(" a", StringUtils.trimTrailingCharacter(" a", ' '));
		assertEquals(" a", StringUtils.trimTrailingCharacter(" a ", ' '));
		assertEquals(" a b", StringUtils.trimTrailingCharacter(" a b ", ' '));
		assertEquals(" a b  c", StringUtils.trimTrailingCharacter(" a b  c ", ' '));
	}
	
	@Test
	public void testCountOccurrencesOf() {
		assertEquals(0, StringUtils.countOccurrencesOf(null, null));
		assertEquals(0, StringUtils.countOccurrencesOf("s", null));
		assertEquals(0, StringUtils.countOccurrencesOf(null, "s"));
		assertEquals(0, StringUtils.countOccurrencesOf("s", ""));
		assertEquals(0, StringUtils.countOccurrencesOf("", "s"));
		
		String s = "erowoiueoiur";
		assertEquals(0, StringUtils.countOccurrencesOf(s, "WERWER"));
		assertEquals(0, StringUtils.countOccurrencesOf(s, "x"));
		assertEquals(0, StringUtils.countOccurrencesOf(s, " "));
		assertEquals(0, StringUtils.countOccurrencesOf(s, ""));
		assertEquals(2, StringUtils.countOccurrencesOf(s, "e"));
		assertEquals(2, StringUtils.countOccurrencesOf(s, "oi"));
		assertEquals(2, StringUtils.countOccurrencesOf(s, "oiu"));
		assertEquals(1, StringUtils.countOccurrencesOf(s, "oiur"));
		assertEquals(2, StringUtils.countOccurrencesOf(s, "r"));
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
	public void testToStringArray() {
		Set<String> set = new TreeSet<String>();
		set.add("1");
		set.add("2");
		set.add("3");
		
		org.junit.Assert.assertArrayEquals(
			new String[]{"1", "2", "3"}, StringUtils.toStringArray(set));
	}
}
