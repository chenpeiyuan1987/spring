package org.yuan.study.spring.beans.propertyeditors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.beans.PropertyEditor;

import org.junit.Test;

public final class ByteArrayPropertyEditorTest {

	@Test
	public void testSunnyDaySetAsText() throws Exception {
		final String text = "1234567890";
		
		PropertyEditor byteEditor = new ByteArrayPropertyEditor();
		byteEditor.setAsText(text);
		
		Object value = byteEditor.getValue();
		assertNotNull(value);
		assertTrue(value instanceof byte[]);
		byte[] bytes = (byte[]) value;
		for (int i=0; i<text.length(); i++) {
			assertEquals(text.charAt(i), bytes[i]);
		}
		assertEquals(text, byteEditor.getAsText());
	}
	
	@Test
	public void testGetAsTextReturnsEmptyStringIfValueIsNull() throws Exception {
		PropertyEditor byteEditor = new ByteArrayPropertyEditor();
		assertEquals("", byteEditor.getAsText());
		
		byteEditor.setAsText(null);
		assertEquals("", byteEditor.getAsText());
	}
}
