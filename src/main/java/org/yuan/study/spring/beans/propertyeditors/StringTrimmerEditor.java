package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;

import org.yuan.study.spring.util.StringUtils;

public class StringTrimmerEditor extends PropertyEditorSupport {

	private String charsToDelete;
	
	private final boolean emptyAsNull;

	/**
	 * Create a new StringTrimmerEditor instance.
	 * @param emptyAsNull
	 */
	public StringTrimmerEditor(boolean emptyAsNull) {
		this.charsToDelete = null;
		this.emptyAsNull = emptyAsNull;
	}
	
	/**
	 * Create a new StringTrimmerEditor instance.
	 * @param charsToDelete
	 * @param emptyAsNull
	 */
	public StringTrimmerEditor(String charsToDelete, boolean emptyAsNull) {
		this.charsToDelete = charsToDelete;
		this.emptyAsNull = emptyAsNull;
	}

	@Override
	public String getAsText() {
		Object value = getValue();
		return (value != null ? value.toString() : "");
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		if (text == null) {
			setValue(null);
		} 
		else {
			String value = text.trim();
			if (this.charsToDelete != null) {
				value = StringUtils.deleteAny(value, this.charsToDelete);
			}
			if (this.emptyAsNull && "".equals(value)) {
				setValue(null);
			} 
			else {
				setValue(value);
			}
		}
	}
}
