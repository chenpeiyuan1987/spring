package org.yuan.study.spring.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
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
	public void testReplace() {
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
	public void testDelete() {
		String inString = "The quick brown fox jumped over the lazy dog";
		
		String noThe = StringUtils.delete(inString, "the");
		assertTrue(noThe.equals("The quick brown fox jumped over  lazy dog"));
		
		String nohe = StringUtils.delete(inString, "he");
		assertTrue(nohe.equals("T quick brown fox jumped over t lazy dog"));
		
		String nosp = StringUtils.delete(inString, " ");
		assertTrue(nosp.equals("Thequickbrownfoxjumpedoverthelazydog"));
		
		String killEnd = StringUtils.delete(inString, "dog");
		assertTrue(killEnd.equals("The quick brown fox jumped over the lazy "));
		
		String mismatch = StringUtils.delete(inString, "dxxcxcxog");
		assertTrue(mismatch.equals(inString));
		
		String nochange = StringUtils.delete(inString, "");
		assertTrue(nochange.equals(inString));
	}
	
	@Test
	public void testDeleteAny() {
		String inString = "Able was I ere I saw Elba";
		
		assertEquals("Able was  ere  saw Elba", StringUtils.deleteAny(inString, "I"));
		assertEquals("l ws I r I sw l", StringUtils.deleteAny(inString, "AeEba!"));
		assertEquals(inString, StringUtils.deleteAny(inString, "#@$#$^"));
		
		inString = "This is\n\n\n    \t   a messagy string with whitespace\n";
		assertTrue(inString.indexOf("\n") != -1);
		assertTrue(inString.indexOf("\t") != -1);
		assertTrue(inString.indexOf(" ") != -1);
		inString = StringUtils.deleteAny(inString, "\n\t ");
		assertTrue(inString.indexOf("\n") == -1);
		assertTrue(inString.indexOf("\t") == -1);
		assertTrue(inString.indexOf(" ") == -1);
		assertTrue(inString.length() > 10);
	}
	
	@Test
	public void testQuote() {
		assertEquals("'myString'", StringUtils.quote("myString"));
		assertEquals("''", StringUtils.quote(""));
		assertNull(StringUtils.quote(null));
	}
	
	@Test
	public void testQuoteIfString() {
		assertEquals("'myString'", StringUtils.quoteIfString("myString"));
		assertEquals("''", StringUtils.quoteIfString(""));
		assertEquals(new Integer(5), StringUtils.quoteIfString(new Integer(5)));
		assertNull(StringUtils.quoteIfString(null));
	}
	
	@Test
	public void testUnqualify() {
		assertEquals("unqualified", StringUtils.unqualify("i.am.not.unqualified"));
	}
	
	@Test
	public void testCapitalize() {
		assertEquals("I am not capitalize", StringUtils.capitalize("i am not capitalize"));
	}
	
	@Test
	public void testUncapitalize() {
		assertEquals("i am capitalize", StringUtils.uncapitalize("I am capitalize"));
	}
	
	@Test
	public void testGetFilename() {
		assertEquals(null, StringUtils.getFilename(null));
		assertEquals("", StringUtils.getFilename(""));
		assertEquals("myfile", StringUtils.getFilename("myfile"));
		assertEquals("myfile", StringUtils.getFilename("mypath/myfile"));
		assertEquals("myfile.", StringUtils.getFilename("myfile."));
		assertEquals("myfile.", StringUtils.getFilename("mypath/myfile."));
		assertEquals("myfile.txt", StringUtils.getFilename("myfile.txt"));
		assertEquals("myfile.txt", StringUtils.getFilename("mypath/myfile.txt"));
	}
	
	@Test
	public void testGetFilenameExtension() {
		assertEquals(null, StringUtils.getFilenameExtension(null));
		assertEquals(null, StringUtils.getFilenameExtension(""));
		assertEquals(null, StringUtils.getFilenameExtension("myfile"));
		assertEquals(null, StringUtils.getFilenameExtension("mypath/myfile"));
		assertEquals(null, StringUtils.getFilenameExtension("/home/user/.m2/settings/myfile"));
		assertEquals("", StringUtils.getFilenameExtension("myfile."));
		assertEquals("", StringUtils.getFilenameExtension("mypath/myfile."));
		assertEquals("txt", StringUtils.getFilenameExtension("myfile.txt"));
		assertEquals("txt", StringUtils.getFilenameExtension("mypath/myfile.txt"));
		assertEquals("txt", StringUtils.getFilenameExtension("/home/user/.m2/settings/myfile.txt"));
	}
	
	@Test
	public void testStripFilenameExtension() {
		assertEquals(null, StringUtils.stripFilenameExtension(null));
		assertEquals("", StringUtils.stripFilenameExtension(""));
		assertEquals("myfile", StringUtils.stripFilenameExtension("myfile"));
		assertEquals("myfile", StringUtils.stripFilenameExtension("myfile."));
		assertEquals("myfile", StringUtils.stripFilenameExtension("myfile.txt"));
		assertEquals("mypath/myfile", StringUtils.stripFilenameExtension("mypath/myfile"));
		assertEquals("mypath/myfile", StringUtils.stripFilenameExtension("mypath/myfile."));
		assertEquals("mypath/myfile", StringUtils.stripFilenameExtension("mypath/myfile.txt"));
		assertEquals("/home/user/.m2/settings/myfile", StringUtils.stripFilenameExtension("/home/user/.m2/settings/myfile"));
		assertEquals("/home/user/.m2/settings/myfile", StringUtils.stripFilenameExtension("/home/user/.m2/settings/myfile."));
		assertEquals("/home/user/.m2/settings/myfile", StringUtils.stripFilenameExtension("/home/user/.m2/settings/myfile.txt"));
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
		assertEquals("/../mypath/myfile", StringUtils.cleanPath("/../mypath/myfile"));
	}
	
	@Test
	public void testPathEquals() {
		
	}
	
	@Test
	public void testConcatenateStringArrays() {
		
	}
	
	@Test
	public void testMergeStringArrays() {
		
	}
	
	@Test
	public void testSortStringArray() {
		
	}
	
	@Test
	public void testRemoveDuplicateStrings() {
		
	}
	
	@Test
	public void testSplitArrayElementsIntoProperties() {
		
	}
	
	@Test
	public void testSplitArrayElementsIntoPropertiesAndDeletedChars() {
		
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
		String[] sa = StringUtils.tokenizeToStringArray("a,b ,c", ",", false, true);
		assertEquals(3, sa.length);
		assertEquals("a", sa[0]);
		assertEquals("b ", sa[1]);
		assertEquals("c", sa[2]);
	}
	
	@Test
	public void testCommaDelimitedListToStringArrayWithNullProducesEmptyArray() {
		String[] sa = StringUtils.commaDelimitedListToStringArray(null);
		assertTrue(sa != null);
		assertTrue(sa.length == 0);
	}
	
	@Test
	public void testCommaDelimitedListToStringArrayWithEmptyStringProducesEmptyArray() {
		String[] sa = StringUtils.commaDelimitedListToStringArray("");
		assertTrue(sa != null);
		assertTrue(sa.length == 0);
	}
	
	private void testStringArrayReverseTransformationMatches(String[] sa) {
		String[] reverse = StringUtils.commaDelimitedListToStringArray(
			StringUtils.arrayToCommaDelimitedString(sa));
		assertEquals(Arrays.asList(sa), Arrays.asList(reverse));
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
	public void testCommaDelimitedListToStringArrayMatchWords() {
		
	}
	
}
