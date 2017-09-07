package org.yuan.study.spring.beans.propertyeditors;

import static org.junit.Assert.*;

import java.io.InputStream;

import org.junit.Test;
import org.springframework.util.ClassUtils;

public final class InputStreamEditorTest {

	@Test(expected=IllegalArgumentException.class)
	public void testCtorWithNullResourceEditor() throws Exception {
		new InputStreamEditor(null);
	}
	
	@Test
	public void testSunnyDay() throws Exception {
		InputStream stream = null;
		try {
			String resource = String.format("classpath:%s/%s.class", 
				ClassUtils.classPackageAsResourcePath(getClass()), ClassUtils.getShortName(getClass()));
			InputStreamEditor editor = new InputStreamEditor();
			editor.setAsText(resource);
			Object value = editor.getValue();
			assertNotNull(value);
			assertTrue(value instanceof InputStream);
			stream = (InputStream) value;
			assertTrue(stream.available() > 0);
		}
		finally {
			if (stream != null) {
				stream.close();
			}
		}
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testWhenResourceDoesNotExist() throws Exception {
		String resource = "classpath:bingo!";
		InputStreamEditor editor = new InputStreamEditor();
		editor.setAsText(resource);
	}
	
	@Test
	public void testGetAsTextReturnsNullByDefault() throws Exception {
		assertNull(new InputStreamEditor().getAsText());
		String resource = String.format("classpath:%s/%s.class", 
			ClassUtils.classPackageAsResourcePath(getClass()), ClassUtils.getShortName(getClass()));
		InputStreamEditor editor = new InputStreamEditor();
		editor.setAsText(resource);
		assertNull(editor.getAsText());
	}
}
