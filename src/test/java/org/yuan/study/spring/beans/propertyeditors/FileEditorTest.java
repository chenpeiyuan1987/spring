package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditor;
import java.io.File;

import static org.junit.Assert.*;

import org.junit.Test;
import org.springframework.util.ClassUtils;

public final class FileEditorTest {

	//@Test
	public void testClassPathFileName() throws Exception {
		PropertyEditor fileEditor = new FileEditor();
		fileEditor.setAsText(String.format("classpath:%s/%s.class", 
			ClassUtils.classPackageAsResourcePath(getClass()), ClassUtils.getShortName(getClass())));
		Object value = fileEditor.getValue();
		assertTrue(value instanceof File);
		File file = (File) value;
		assertTrue(file.exists());
	}
	
	//@Test(expected=IllegalArgumentException.class)
	public void testWithNonExistentResource() throws Exception {
		PropertyEditor propertyEditor = new FileEditor();
		propertyEditor.setAsText("classpath:no_way_this_file_is_found.doc");
	}
	
	@Test
	public void testWithNonExistentFile() throws Exception {
		PropertyEditor editor = new FileEditor();
		editor.setAsText("file:no_way_this_file_is_found.doc");
		Object value = editor.getValue();
		assertTrue(value instanceof File);
		File file = (File) value;
		assertTrue(!file.exists());
	}
	
	@Test
	public void testAbsoluteFileName() throws Exception {
		PropertyEditor editor = new FileEditor();
		editor.setAsText("/no_way_this_file_is_found.doc");
		Object value = editor.getValue();
		assertTrue(value instanceof File);
		File file = (File) value;
		assertTrue(!file.exists());
	}
	
	//@Test
	public void testUnqualifiedFileNameFound() throws Exception {
		PropertyEditor editor = new FileEditor();
		String fileName = String.format("%s/%s.class", 
			ClassUtils.classPackageAsResourcePath(getClass()), ClassUtils.getShortName(getClass()));
		editor.setAsText(fileName);
		Object value = editor.getValue();
		assertTrue(value instanceof File);
		File file = (File) value;
		assertTrue(file.exists());
		String absolutePath = file.getAbsolutePath();
		if (File.separatorChar == '\\') {
			absolutePath = absolutePath.replace('\\', '/');
		}
		assertTrue(absolutePath.endsWith(fileName));
	}
	
	//@Test
	public void testUnqualifiedFileNameNotFound() throws Exception {
		PropertyEditor editor = new FileEditor();
		String fileName = String.format("%s/%s.clazz", 
			ClassUtils.classPackageAsResourcePath(getClass()), ClassUtils.getShortName(getClass()));
		editor.setAsText(fileName);
		Object value = editor.getValue();
		assertTrue(value instanceof File);
		File file = (File) value;
		assertFalse(file.exists());
		String absolutePath = file.getAbsolutePath();
		if (File.separatorChar == '\\') {
			absolutePath = absolutePath.replace('\\', '/');
		}
		assertTrue(absolutePath.endsWith(fileName));
	}
	
}
