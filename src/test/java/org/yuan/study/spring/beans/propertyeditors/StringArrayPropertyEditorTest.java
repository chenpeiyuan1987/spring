package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditor;

import static org.junit.Assert.*;
import org.junit.Test;

public class StringArrayPropertyEditorTest {

	@Test
	public void testWithDefaultSeparator() {
		PropertyEditor editor = new StringArrayPropertyEditor();
		editor.setAsText("1,2,3");
		Object value = editor.getValue();
		assertNotNull(value);
		assertTrue(value instanceof String[]);
		String[] array = (String[])value;
		for (int i = 0; i < array.length; i++) {
			assertEquals(String.valueOf(i+1), array[i]);
		}
		assertEquals("1,2,3", editor.getAsText());
	}
	
	@Test
	public void testNoTrim() {
		PropertyEditor editor = new StringArrayPropertyEditor();
		editor.setAsText("  1,2  , 3 ");
		Object value = editor.getValue();
		String[] array = (String[])value;
		for (int i = 0; i < array.length; i++) {
			assertEquals(3, array[i].length());
			assertEquals(String.valueOf(i+1), array[i].trim());
		}
		assertEquals("  1,2  , 3 ", editor.getAsText());
	}
	
	@Test
	public void testWithCustomSeparator() {
		PropertyEditor editor = new StringArrayPropertyEditor(":");
		editor.setAsText("1:2:3");
		Object value = editor.getValue();
		assertNotNull(value);
		assertTrue(value instanceof String[]);
		String[] array = (String[])value;
		for (int i = 0; i < array.length; i++) {
			assertEquals(String.valueOf(i+1), array[i]);
		}
		assertEquals("1:2:3", editor.getAsText());
	}
	
	@Test
	public void testWithEmptyArray() {
		PropertyEditor editor = new StringArrayPropertyEditor();
		editor.setAsText("");
		Object value = editor.getValue();
		assertTrue(value instanceof String[]);
		assertEquals(0, ((String[]) value).length);
	}
}
