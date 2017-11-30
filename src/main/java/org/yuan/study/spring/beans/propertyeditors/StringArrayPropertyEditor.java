package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;

import org.yuan.study.spring.util.ObjectUtils;
import org.yuan.study.spring.util.StringUtils;

public class StringArrayPropertyEditor extends PropertyEditorSupport {

	private static final String DEFAULT_SEPARATOR = ",";
	
	private final String separator;
	
	private final String charsToDelete;
	
	private final boolean emptyArrayAsNull;
	
	private final boolean trimValues;

	/**
	 * Create a new StringArrayPropertyEditor with the default separator: a comma
	 */
	public StringArrayPropertyEditor() {
		this(DEFAULT_SEPARATOR, null, false);
	}
	
	/**
	 * Create a new StringArrayPropertyEditor with the default separator.
	 * @param separator
	 */
	public StringArrayPropertyEditor(String separator) {
		this(separator, null, false);
	}
	
	/**
	 * Create a new StringArrayPropertyEditor with the default separator.
	 * @param separator
	 */
	public StringArrayPropertyEditor(String separator, boolean emptyArrayAsNull) {
		this(separator, null, emptyArrayAsNull);
	}
	
	/**
	 * Create a new StringArrayPropertyEditor with the default separator.
	 * @param separator
	 */
	public StringArrayPropertyEditor(String separator, boolean emptyArrayAsNull, boolean trimValues) {
		this(separator, null, emptyArrayAsNull, trimValues);
	}
	
	/**
	 * Create a new StringArrayPropertyEditor with the default separator.
	 * @param separator
	 */
	public StringArrayPropertyEditor(String separator, String charsToDelete, boolean emptyArrayAsNull) {
		this(separator, charsToDelete, emptyArrayAsNull, true);
	}

	/**
	 * Create a new StringArrayPropertyEditor with the default separator.
	 * @param separator
	 */
	public StringArrayPropertyEditor(String separator, String charsToDelete, boolean emptyArrayAsNull, boolean trimValues) {
		this.separator = separator;
		this.charsToDelete = charsToDelete;
		this.emptyArrayAsNull = emptyArrayAsNull;
		this.trimValues = trimValues;
	}

	@Override
	public String getAsText() {
		return StringUtils.arrayToDelimitedString(ObjectUtils.toObjectArray(getValue()), separator);
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		String[] array = StringUtils.delimitedListToStringArray(text, separator, charsToDelete);
		if (trimValues) {
			array = StringUtils.trimArrayElements(array);
		}
		if (emptyArrayAsNull && array.length == 0) {
			setValue(null);
		}
		else {
			setValue(array);
		}
	}
	
}
