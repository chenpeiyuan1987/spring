package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;

import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.StringUtils;

public class ClassEditor extends PropertyEditorSupport {

	private final ClassLoader classLoader;

	/**
	 * 
	 */
	public ClassEditor() {
		this(null);
	}

	/**
	 * 
	 * @param classLoader
	 */
	public ClassEditor(ClassLoader classLoader) {
		this.classLoader = (classLoader != null ? classLoader : ClassUtils.getDefaultClassLoader());
	}
	
	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		if (StringUtils.hasText(text)) {
			try {
				setValue(ClassUtils.forName(text.trim(), this.classLoader));
			}
			catch (ClassNotFoundException ex) {
				throw new IllegalArgumentException("Class not found: " + ex.getMessage());
			}
		}
		else {
			setValue(null);
		}
	}
	
	@Override
	public String getAsText() {
		Class<?> clazz = (Class<?>) getValue();
		if (clazz != null) {
			return ClassUtils.getQualifiedName(clazz);
		}
		else {
			return "";
		}
	}
}
