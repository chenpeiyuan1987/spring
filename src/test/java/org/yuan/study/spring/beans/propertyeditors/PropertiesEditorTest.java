package org.yuan.study.spring.beans.propertyeditors;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.junit.Test;

public final class PropertiesEditorTest {

	@Test
	public void testOneProperty() {
		String s = "foo=bar";
		PropertiesEditor pe = new PropertiesEditor();
		pe.setAsText(s);
		Properties p = (Properties) pe.getValue();
		assertTrue(p.entrySet().size() == 1);
		assertTrue(p.get("foo").equals("bar"));
	}
	
	@Test
	public void testTwoProperties() {
		String s = "foo=bar\nme=mi";
		PropertiesEditor pe = new PropertiesEditor();
		pe.setAsText(s);
		Properties p = (Properties) pe.getValue();
		assertTrue(p.entrySet().size() == 2);
		assertTrue(p.get("foo").equals("bar"));
		assertTrue(p.get("me").equals("mi"));
	}
	
	@Test
	public void testHandlesEqualsInValue() {
		String s = "foo=bar\nme=mi\nx=y=z";
		PropertiesEditor pe = new PropertiesEditor();
		pe.setAsText(s);
		Properties p = (Properties) pe.getValue();
		assertTrue(p.entrySet().size() == 3);
		assertTrue(p.get("foo").equals("bar"));
		assertTrue(p.get("me").equals("mi"));
		assertTrue(p.get("x").equals("y=z"));
	}
	
	@Test
	public void testHandlesEmptyProperty() {
		String s = "foo=bar\nme=mi\nx=";
		PropertiesEditor pe = new PropertiesEditor();
		pe.setAsText(s);
		Properties p = (Properties) pe.getValue();
		assertTrue(p.entrySet().size() == 3);
		assertTrue(p.get("foo").equals("bar"));
		assertTrue(p.get("me").equals("mi"));
		assertTrue(p.get("x").equals(""));
	}
	
	@Test
	public void testHandlesEmptyPropertyWithoutEquals() {
		String s = "foo\nme=mi\nx=x";
		PropertiesEditor pe = new PropertiesEditor();
		pe.setAsText(s);
		Properties p = (Properties) pe.getValue();
		assertTrue(p.entrySet().size() == 3);
		assertTrue(p.get("foo").equals(""));
		assertTrue(p.get("me").equals("mi"));
		assertTrue(p.get("x").equals("x"));
	}
	
	@Test
	public void testIgnoresCommentLinesAndEmptyLines() {
		String s = "#Ignore this comment\n"
			+ "foo=bar\n"
			+ "#Another=comment more junk /\n"
			+ "me=mi\n"
			+ "x=x\n"
			+ "\n";
		PropertiesEditor pe = new PropertiesEditor();
		pe.setAsText(s);
		Properties p = (Properties) pe.getValue();
		assertTrue(p.entrySet().size() == 3);
		assertTrue(p.get("foo").equals("bar"));
		assertTrue(p.get("me").equals("mi"));
		assertTrue(p.get("x").equals("x"));
	}
	
	@Test
	public void testIgnoresLeadingSpacesAndTabs() {
		String s = "#Ignore this comment\n"
				+ "foo=bar\n"
				+ "#Another=comment more junk /\n"
				+ "me=mi\n"
				+ "x=x\n"
				+ "\n";
		PropertiesEditor pe = new PropertiesEditor();
		pe.setAsText(s);
		Properties p = (Properties) pe.getValue();
		assertTrue(p.entrySet().size() == 3);
		assertTrue(p.get("foo").equals("bar"));
		assertTrue(p.get("me").equals("mi"));
		assertTrue(p.get("x").equals("x"));
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testNull() {
		PropertiesEditor editor = new PropertiesEditor();
		editor.setAsText(null);
	}
	
	@Test
	public void testEmptyString() {
		PropertiesEditor editor = new PropertiesEditor();
		editor.setAsText("");
		Properties props = (Properties) editor.getValue();
		assertTrue(props.isEmpty());
	}
	
	@Test
	public void testUsingMapAsValueSource() throws Exception {
		Map map = new HashMap();
		map.put("one", "1");
		map.put("two", "2");
		map.put("three", "3");
		
		PropertiesEditor editor = new PropertiesEditor();
		editor.setValue(map);
		Object value = editor.getValue();
		assertNotNull(value);
		assertTrue(value instanceof Properties);
		Properties props = (Properties) value;
		assertEquals(3, props.size());
		assertEquals("1", props.getProperty("one"));
		assertEquals("2", props.getProperty("two"));
		assertEquals("3", props.getProperty("three"));
	}
	
}
