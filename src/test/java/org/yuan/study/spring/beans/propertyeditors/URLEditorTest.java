package org.yuan.study.spring.beans.propertyeditors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.beans.PropertyEditor;
import java.net.URL;

import org.junit.Test;
import org.yuan.study.spring.util.ClassUtils;

public class URLEditorTest {

	@Test
	public void testStandardURI() throws Exception {
		PropertyEditor editor = new URLEditor();
		editor.setAsText("mailto:juergen.hoeller@interface21.com");
		Object value = editor.getValue();
		assertTrue(value instanceof URL);
		URL url = (URL) value;
		assertEquals(url.toExternalForm(), editor.getAsText());
	}
	
	@Test
	public void testStandardURL() throws Exception {
		PropertyEditor editor = new URLEditor();
		editor.setAsText("http://www.springframework.org");
		Object value = editor.getValue();
		assertTrue(value instanceof URL);
		URL url = (URL) value;
		assertEquals(url.toExternalForm(), editor.getAsText());
	}
	
	@Test
	public void testClasspathURL() throws Exception {
		PropertyEditor editor = new URLEditor();
		editor.setAsText("classpath:" + 
				ClassUtils.classPackageAsResourcePath(getClass()) + 
				"/" + ClassUtils.getShortName(getClass()) + ".class");
		Object value = editor.getValue();
		assertTrue(value instanceof URL);
		URL url = (URL) value;
		assertEquals(url.toExternalForm(), editor.getAsText());
		assertTrue(!url.getProtocol().startsWith("classpath"));
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testWithNonExistentResource() throws Exception {
		PropertyEditor editor = new URLEditor();
		editor.setAsText("gonna:/freak/in/the/morning/freak/in/the.evening");
	}
	
	@Test
	public void testSetAsTextwithNull() throws Exception {
		PropertyEditor editor = new URLEditor();
		editor.setAsText(null);
		assertNull(editor.getValue());
		assertEquals("", editor.getAsText());
	}
	
	@Test
	public void testGetAsTextReturnsEmptyStringIfValueNotSet() throws Exception {
		assertEquals("", new URLEditor().getAsText());
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testCtorWithNullResourceEditor() throws Exception {
		new URLEditor(null);
	}
}
