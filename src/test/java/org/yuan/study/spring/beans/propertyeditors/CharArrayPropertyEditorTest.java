package org.yuan.study.spring.beans.propertyeditors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.beans.PropertyEditor;

import org.junit.Test;

public final class CharArrayPropertyEditorTest {

	@Test
	public void testSunnyDaySetAsText() throws Exception {
		final String text = "1234567890";
		
		PropertyEditor charEditor = new CharArrayPropertyEditor();
		charEditor.setAsText(text);
		
		Object value = charEditor.getValue();
		assertNotNull(value);
		assertTrue(value instanceof char[]);
		char[] bytes = (char[]) value;
		for (int i=0; i<text.length(); i++) {
			assertEquals(text.charAt(i), bytes[i]);
		}
		assertEquals(text, charEditor.getAsText());
	}
	
	@Test
	public void testGetAsTextReturnsEmptyStringIfValueIsNull() throws Exception {
		PropertyEditor charEditor = new CharArrayPropertyEditor();
		assertEquals("", charEditor.getAsText());
		
		charEditor.setAsText(null);
		assertEquals("", charEditor.getAsText());
	}
}
