package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;

import org.yuan.study.spring.util.StringUtils;

public class StringArrayPropertyEditor extends PropertyEditorSupport {

	private static final String DEFAULT_SEPARATOR = ",";
	
	private final String separator;

	/**
	 * Create a new StringArrayPropertyEditor with the default separator: a comma
	 */
	public StringArrayPropertyEditor() {
		this.separator = DEFAULT_SEPARATOR;
	}
	
	/**
	 * Create a new StringArrayPropertyEditor with the default separator.
	 * @param separator
	 */
	public StringArrayPropertyEditor(String separator) {
		this.separator = separator;
	}

	@Override
	public String getAsText() {
		String[] array = (String[]) getValue();
		return StringUtils.arrayToDelimitedString(array, separator);
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		String[] array = StringUtils.delimitedListToStringArray(text, separator);
		setValue(array);
	}
	
}
