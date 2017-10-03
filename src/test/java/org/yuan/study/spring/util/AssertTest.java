package org.yuan.study.spring.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Test;

/**
 * Unit tests for the Assert class.
 * @author Yuan
 */
public class AssertTest {

	@Test(expected=IllegalArgumentException.class)
	public void testIsInstanceOf() {
		final Set<Object> set = new HashSet<Object>();
		Assert.isInstanceOf(HashSet.class, set);
		Assert.isInstanceOf(HashMap.class, set);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testIsInstanceOfClassWithNullInstanceThrowsException() {
		Assert.isInstanceOf(String.class, null);
	}
	
	@Test
	public void testIsNullDoesNotThrowExceptionIfArgumentIsNullWithMessage() {
		Assert.isNull(null, "Bla");
	}
	
	@Test
	public void testIsNullDoesNotThrowExceptionIfArgumentIsNull() {
		Assert.isNull(null);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testIsNullThrowsExceptionIfArgumentIsNotNull() {
		Assert.isNull(new Object());
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testIsTrueWithFalseExpressionThrowsException() {
		Assert.isTrue(false);
	}
	
	@Test
	public void testIsTrueWithTrueExpressionSunnyDay() {
		Assert.isTrue(true);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testHasLengthWithNullStringThrowsException() {
		Assert.hasLength(null);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testHasLengthWithEmptyStringThrowsException() {
		Assert.hasLength("");
	}
	
	@Test
	public void testHasLengthWithWhitespaceOnlyStringDoesNotThrowException() {
		Assert.hasLength("\t    ");
	}
	
	@Test
	public void testHasLengthSunnyDay() {
		Assert.hasLength("I heart ...");
	}
	
	@Test
	public void testDoesNotContainWithNullSearchStringDoesNotThrowException() {
		Assert.doesNotContain(null, "rod");
	}
	
	@Test
	public void testDoesNotContainWithNullSubstringDoesNotThrowException() {
		Assert.doesNotContain("A cool chick's name is Brod.", null);
	}
	
	@Test
	public void testDoesNotContainWithEmptySubstringDoesNotThrowException() {
		Assert.doesNotContain("A cool chick's name is Brod.", "");
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testAssertNotEmptyWithNullCollectionThrowsException() {
		Assert.notEmpty((Collection<Object>)null);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testAssertNotEmptyWithEmptyCollectionThrowsException() {
		Assert.notEmpty(new ArrayList<Object>());
	}
	
	@Test
	public void testAssertNotEmptyWithCollectionSunnyDay() {
		List<String> collection = new ArrayList<String>();
		collection.add("");
		Assert.notEmpty(collection);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testAssertNotEmptyWithNullMapThrowsException() {
		Assert.notEmpty((Map<String,String>)null);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testAssertNotEmptyWithEmptyMapThrowsException() {
		Assert.notEmpty(new HashMap<String,String>());
	}
	
	@Test
	public void testAssertNotEmptyWithMapSunnyDay() {
		Map<String,String> map = new HashMap<String,String>();
		map.put("", "");
		Assert.notEmpty(map);
	}
	
	@Test(expected=IllegalStateException.class)
	public void testStateWithFalseExpressionThrowsException() {
		Assert.state(false);
	}
	
	@Test
	public void testStateWithTrueExpressionSunnyDay() {
		Assert.state(true);
	}
}
