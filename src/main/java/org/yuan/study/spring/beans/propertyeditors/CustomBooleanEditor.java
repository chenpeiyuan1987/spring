package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;

import org.yuan.study.spring.util.StringUtils;

public class CustomBooleanEditor extends PropertyEditorSupport {

	public static final String VALUE_TRUE = "true";
	public static final String VALUE_FALSE = "false";
	
	public static final String VALUE_ON = "on";
	public static final String VALUE_OFF = "off";
	
	public static final String VALUE_YES = "yes";
	public static final String VALUE_NO = "no";
	
	public static final String VALUE_1 = "1";
	public static final String VALUE_0 = "0";
	
	private final String trueString;
	
	private final String falseString;
	
	private final boolean allowEmpty;
	
	/**
	 * Create a new CustomBooleanEditor instance,
	 * with configurable String values for true and false.
	 * @param allowEmpty
	 */
	public CustomBooleanEditor(boolean allowEmpty) {
		this(null, null, allowEmpty);
	}

	/**
	 * Create a new CustomBooleanEditor instance,
	 * with configurable String values for true and false.
	 * @param trueString
	 * @param falseString
	 * @param allowEmpty
	 */
	public CustomBooleanEditor(String trueString, String falseString, boolean allowEmpty) {
		this.trueString = trueString;
		this.falseString = falseString;
		this.allowEmpty = allowEmpty;
	}

	@Override
	public String getAsText() {
		if (Boolean.TRUE.equals(getValue())) {
			return (this.trueString != null ? this.trueString : VALUE_TRUE);
		} 
		else if (Boolean.FALSE.equals(getValue())) {
			return (this.falseString != null ? this.falseString : VALUE_FALSE);
		}
		else {
			return "";
		}
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		if (this.allowEmpty && !StringUtils.hasText(text)) {
			setValue(null);
		} 
		else if (this.trueString != null && text.equalsIgnoreCase(this.trueString)) {
			setValue(Boolean.TRUE);
		}
		else if (this.falseString != null && text.equalsIgnoreCase(this.falseString)) {
			setValue(Boolean.FALSE);
		}
		else if (this.trueString == null && 
				(text.equalsIgnoreCase(VALUE_TRUE) || text.equalsIgnoreCase(VALUE_ON) || 
				text.equalsIgnoreCase(VALUE_YES) || text.equalsIgnoreCase(VALUE_1))) {
			setValue(Boolean.TRUE);
		}
		else if (this.falseString == null && 
				(text.equalsIgnoreCase(VALUE_FALSE) || text.equalsIgnoreCase(VALUE_OFF) || 
				text.equalsIgnoreCase(VALUE_NO) || text.equalsIgnoreCase(VALUE_0))) {
			setValue(Boolean.FALSE);
		}
		else {
			throw new IllegalArgumentException(String.format("Invalid boolean value [%s]", text));
		}
	}

}
