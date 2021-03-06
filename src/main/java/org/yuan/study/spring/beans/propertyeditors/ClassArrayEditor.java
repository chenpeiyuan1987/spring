package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;

import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.ObjectUtils;
import org.yuan.study.spring.util.StringUtils;

public class ClassArrayEditor extends PropertyEditorSupport {

	private final ClassLoader classLoader;

	/**
	 * 
	 */
	public ClassArrayEditor() {
		this(null);
	}

	/**
	 * 
	 * @param classLoader
	 */
	public ClassArrayEditor(ClassLoader classLoader) {
		this.classLoader = (classLoader != null ? classLoader : ClassUtils.getDefaultClassLoader());
	}
	
	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		if (StringUtils.hasText(text)) {
			String[] classNames = StringUtils.commaDelimitedListToStringArray(text);
			Class[] classes = new Class[classNames.length];
			for (int i = 0; i < classes.length; i++) {
				String className = classNames[i].trim();
				classes[i] = ClassUtils.resolveClassName(className, classLoader);
			}
			setValue(classes);
		}
		else {
			setValue(null);
		}
	}
	
	@Override
	public String getAsText() {
		Class[] classes = (Class[]) getValue();
		if (ObjectUtils.isEmpty(classes)) {
			return "";
		}
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < classes.length; i++) {
			if (i > 0) {
				sb.append(",");
			}
			sb.append(ClassUtils.getQualifiedName(classes[i]));
		}
		return sb.toString();
	}
}
