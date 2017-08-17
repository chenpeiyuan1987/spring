package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;
import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;

import org.springframework.util.StringUtils;

public class CustomDateEditor extends PropertyEditorSupport {

	private final DateFormat dateFormat;
	
	private final boolean allowEmpty;
	
	private final int exactDateLength;

	/**
	 * Create a new CustomDateEditor instance, 
	 * using the given DateFormat for parsing and rendering.
	 * @param dateFormat
	 * @param allowEmpty
	 */
	public CustomDateEditor(DateFormat dateFormat, boolean allowEmpty) {
		this.dateFormat = dateFormat;
		this.allowEmpty = allowEmpty;
		this.exactDateLength = -1;
	}
	
	/**
	 * Create a new CustomDateEditor instance, 
	 * using the given DateFormat for parsing and rendering.
	 * @param dateFormat
	 * @param allowEmpty
	 * @param exactDateLength
	 */
	public CustomDateEditor(DateFormat dateFormat, boolean allowEmpty, int exactDateLength) {
		this.dateFormat = dateFormat;
		this.allowEmpty = allowEmpty;
		this.exactDateLength = exactDateLength;
	}

	@Override
	public String getAsText() {
		Date value = (Date) getValue();
		return (value != null ? dateFormat.format(value) : "");
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		if (allowEmpty && !StringUtils.hasText(text)) {
			setValue(null);
		} 
		else if (text != null && exactDateLength >= 0 && text.length() != exactDateLength) {
			throw new IllegalArgumentException(String.format("Could not parse date: it is not exactly %s characters long", exactDateLength));
		}
		else {
			try {
				setValue(dateFormat.parse(text));
			}
			catch (ParseException ex) {
				throw new IllegalArgumentException(String.format("Could not parse date: %s", ex.getMessage()));
			}
		}
	}
	
	
}
