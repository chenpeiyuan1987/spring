package org.yuan.study.spring.util;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Arrays;
import java.util.Locale;
import java.util.Properties;

import org.junit.Test;

public final class StringUtilsTest {

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
		assertTrue(StringUtils.pathEquals("/dummy1/dummy2/dummy3", "/dummy1/dummy2/dummy3"));
		assertTrue(StringUtils.pathEquals("C:\\dummy1\\dummy2\\dummy3", "C:\\dummy1\\dummy2\\dummy3"));
		assertTrue(StringUtils.pathEquals("/dummy1/bin/../dummy2/dummy3", "/dummy1/dummy2/dummy3"));
		assertTrue(StringUtils.pathEquals("C:\\dummy1\\dummy2\\dummy3", "C:\\dummy1\\bin\\..\\dummy2\\dummy3"));
		assertTrue(StringUtils.pathEquals("/dummy1/bin/../dummy2/bin/../dummy3", "/dummy1/dummy2/dummy3"));
		assertTrue(StringUtils.pathEquals("C:\\dummy1\\dummy2\\dummy3", "C:\\dummy1\\bin\\..\\dummy2\\bin\\..\\dummy3"));
		assertTrue(StringUtils.pathEquals("/dummy1/bin/tmp/../../dummy2/dummy3", "/dummy1/dummy2/dummy3"));
		assertTrue(StringUtils.pathEquals("/dummy1/dummy2/dummy3", "/dummy1/dum/dum/../../dummy2/dummy3"));
		assertTrue(StringUtils.pathEquals("./dummy1/dummy2/dummy3", "dummy1/dum/./dum/../../dummy2/dummy3"));
		assertFalse(StringUtils.pathEquals("./dummy1/dummy2/dummy3", "/dummy1/dum/./dum/../../dummy2/dummy3"));
		assertFalse(StringUtils.pathEquals("/dummy1/dummy2/dummy3", "/dummy1/dummy4/dummy3"));
		assertFalse(StringUtils.pathEquals("/dummy1/bin/tmp/../dummy2/dummy3", "/dummy1/dummy2/dummy3"));
		assertFalse(StringUtils.pathEquals("C:\\dummy1\\dummy2\\dummy3", "C:\\dummy1\\bin\\tmp\\..\\dummy2\\dummy3"));
		assertFalse(StringUtils.pathEquals("/dummy1/bin/../dummy2/dummy3", "/dummy1/dummy2/dummy4"));
	}
	
	@Test
	public void testConcatenateStringArrays() {
		String[] input1 = new String[] {"myString2"};
		String[] input2 = new String[] {"myString1", "myString2"};
		String[] result = StringUtils.concatenateStringArrays(input1, input2);
		assertEquals(3, result.length);
		assertEquals("myString2", result[0]);
		assertEquals("myString1", result[1]);
		assertEquals("myString2", result[2]);
		
		assertNull(StringUtils.concatenateStringArrays(null, null));
		assertArrayEquals(input1, StringUtils.concatenateStringArrays(input1, null));
		assertArrayEquals(input2, StringUtils.concatenateStringArrays(null, input2));
	}
	
	@Test
	public void testMergeStringArrays() {
		String[] input1 = new String[] {"myString2"};
		String[] input2 = new String[] {"myString1", "myString2"};
		String[] result = StringUtils.mergeStringArrays(input1, input2);
		assertEquals(2, result.length);
		assertEquals("myString2", result[0]);
		assertEquals("myString1", result[1]);
		
		assertNull(StringUtils.mergeStringArrays(null, null));
		assertArrayEquals(input1, StringUtils.mergeStringArrays(input1, null));
		assertArrayEquals(input2, StringUtils.mergeStringArrays(null, input2));
	}
	
	@Test
	public void testSortStringArray() {
		String[] input = new String[] {"myString2"};
		input = StringUtils.addStringToArray(input, "myString1");
		assertEquals("myString2", input[0]);
		assertEquals("myString1", input[1]);
		
		StringUtils.sortStringArray(input);
		assertEquals("myString1", input[0]);
		assertEquals("myString2", input[1]);
	}
	
	@Test
	public void testRemoveDuplicateStrings() {
		String[] input = new String[] {"myString2", "myString1", "myString2"};
		input = StringUtils.removeDuplicateStrings(input);
		assertEquals("myString1", input[0]);
		assertEquals("myString2", input[1]);
	}
	
	@Test
	public void testSplitArrayElementsIntoProperties() {
		String[] input = new String[] {"key1=value1", "key2 =\"value2\""};
		Properties result = StringUtils.splitArrayElementsIntoProperties(input, "=");
		assertEquals("value1", result.getProperty("key1"));
		assertEquals("\"value2\"", result.getProperty("key2"));
	}
	
	@Test
	public void testSplitArrayElementsIntoPropertiesAndDeletedChars() {
		String[] input = new String[] {"key1=value1", "key2 =\"value2\""};
		Properties result = StringUtils.splitArrayElementsIntoProperties(input, "=", "\"");
		assertEquals("value1", result.getProperty("key1"));
		assertEquals("value2", result.getProperty("key2"));
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
		String[] sa = new String[] {"foo", "bar", "big"};
		doTestCommaDelimitedListToStringArrayLegalMatch(sa);
		testStringArrayReverseTransformationMatches(sa);
		
		sa = new String[] {"a", "b", "c"};
		doTestCommaDelimitedListToStringArrayLegalMatch(sa);
		testStringArrayReverseTransformationMatches(sa);
		
		sa = new String[] {"AA", "AA", "AA", "AA", "AA"};
		doTestCommaDelimitedListToStringArrayLegalMatch(sa);
		testStringArrayReverseTransformationMatches(sa);
	}
	
	@Test
	public void testCommaDelimitedListToStringArraySingleString() {
		String s = "woeirqupoiewuropqiewuorpqiwueopriquwopeiurqopwieur";
		String[] sa = StringUtils.commaDelimitedListToStringArray(s);
		assertTrue(sa.length == 1);
		assertTrue(sa[0].equals(s));
	}
	
	@Test
	public void testCommaDelimitedListToStringArrayWithOtherPunctuation() {
		String[] sa = new String[] {"xcvwert4456346&*.", "///", ".!", ".", ";"};
		doTestCommaDelimitedListToStringArrayLegalMatch(sa);
	}
	
	@Test
	public void testCommaDelimitedListToStringArrayEmptyStrings() {
		String[] sa = StringUtils.commaDelimitedListToStringArray("a,,b");
		assertEquals(3, sa.length);
		assertTrue(sa[0].equals("a"));
		assertTrue(sa[1].equals(""));
		assertTrue(sa[2].equals("b"));
		
		sa = new String[] {"", "", "a", ""};
		doTestCommaDelimitedListToStringArrayLegalMatch(sa);
	}
	
	private void doTestCommaDelimitedListToStringArrayLegalMatch(String[] components) {
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < components.length; i++) {
			if (i > 0) {
				sb.append(",");
			}
			sb.append(components[i]);
		}
		
		String[] sa = StringUtils.commaDelimitedListToStringArray(sb.toString());
		assertTrue(sa != null);
		assertTrue(components.length == sa.length);
		assertTrue(Arrays.equals(sa, components));
	}
	
	@Test
	public void testEndsWithIgnoreCase() {
		String suffix = "fOo";
		assertTrue(StringUtils.endsWithIgnoreCase("foo", suffix));
		assertTrue(StringUtils.endsWithIgnoreCase("Foo", suffix));
		assertTrue(StringUtils.endsWithIgnoreCase("barfoo", suffix));
		assertTrue(StringUtils.endsWithIgnoreCase("barbarfoo", suffix));
		assertTrue(StringUtils.endsWithIgnoreCase("barFoo", suffix));
		assertTrue(StringUtils.endsWithIgnoreCase("barBarFoo", suffix));
		assertTrue(StringUtils.endsWithIgnoreCase("barfoO", suffix));
		assertTrue(StringUtils.endsWithIgnoreCase("barFOO", suffix));
		assertTrue(StringUtils.endsWithIgnoreCase("barfOo", suffix));
		assertFalse(StringUtils.endsWithIgnoreCase(null, suffix));
		assertFalse(StringUtils.endsWithIgnoreCase("barfOo", null));
		assertFalse(StringUtils.endsWithIgnoreCase("b", suffix));
	}
	
	@Test
	public void testParseLocaleStringSunnyDay() {
		Locale expectedLocale = Locale.UK;
		Locale locale = StringUtils.parseLocaleString(expectedLocale.toString());
		assertNotNull(locale);
		assertEquals(expectedLocale, locale);
	}
	
	@Test
	public void testParseLocaleStringWithMalformedLocaleString() {
		Locale locale = StringUtils.parseLocaleString("_banjo_on_my_knee");
		assertNotNull(locale);
	}
	
	@Test
	public void testParseLocaleStringWithEmptyLocaleStringYieldsNullLocale() {
		Locale locale = StringUtils.parseLocaleString("");
		assertNull(locale);
	}
	
	@Test
	public void testParseLocaleWithMultiSpecialCharactersInVariant() {
		String variant = "proper-northern";
		String localeString = "en_GB_" + variant;
		Locale locale = StringUtils.parseLocaleString(localeString);
		assertEquals(variant, locale.getVariant());
	}
	
	@Test
	public void testParseLocaleWithMultiValuedVariant() {
		String variant = "proper_northern";
		String localeString = "en_GB_" + variant;
		Locale locale = StringUtils.parseLocaleString(localeString);
		assertEquals(variant, locale.getVariant());
	}
	
	@Test
	public void testParseLocaleWithMultiValuedVariantUsingSpacesAsSeparators() {
		String variant = "proper northern";
		String localeString = "en GB " + variant;
		Locale locale = StringUtils.parseLocaleString(localeString);
		assertEquals(variant, locale.getVariant());
	}
	
	@Test
	public void testParseLocaleWithMultiValuedVariantUsingMixtureOfUnderscoresAndSpacesAsSeparators() {
		String variant = "proper northern";
		String localeString = "en_GB_" + variant;
		Locale locale = StringUtils.parseLocaleString(localeString);
		assertEquals(variant, locale.getVariant());
	}
	
	@Test
	public void testParseLocaleWithMultiValuedVariantUsingSpacesAsSeparatorsWithLotsOfLeadingWhitespace() {
		String variant = "proper northern";
		String localeString = "en GB             " + variant;
		Locale locale = StringUtils.parseLocaleString(localeString);
		assertEquals(variant, locale.getVariant());
	}
	
	@Test
	public void testParseLocaleWithMultiValuedVariantUsingUnderscoresAsSeparatorsWithLotsOfLeadingWhitespace() {
		String variant = "proper_northern";
		String localeString = "en_GB______" + variant;
		Locale locale = StringUtils.parseLocaleString(localeString);
		assertEquals(variant, locale.getVariant());
	}
	
	@Test
	public void testParseLocaleWithInvalidCharacters() {
		try {
			StringUtils.parseLocaleString("%0D%0AContent-length:30%0D%0A%0D%0A%3Cscript%3Ealert%28123%29%3C/script%3E");
			fail("Should have thrown IllegalArgumentException");
		}
		catch (IllegalArgumentException ex) {}
	}
}
