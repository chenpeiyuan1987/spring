package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;
import java.text.NumberFormat;

import org.springframework.util.NumberUtils;
import org.springframework.util.StringUtils;

public class CustomNumberEditor extends PropertyEditorSupport {

	private final Class<?> numberClass;
	
	private final NumberFormat numberFormat;
	
	private final boolean allowEmpty;
	
	/**
	 * Create a new CustomNumberEditor instance, 
	 * using the given Numberformat for parsing and rendering
	 * @param numberClass
	 * @param allowEmpty
	 * @throws IllegalArgumentException
	 */
	public CustomNumberEditor(Class<?> numberClass, boolean allowEmpty) 
		throws IllegalArgumentException {
		this(numberClass, null, allowEmpty);
	}

	/**
	 * Create a new CustomNumberEditor instance, 
	 * using the given Numberformat for parsing and rendering
	 * @param numberClass
	 * @param numberFormat
	 * @param allowEmpty
	 * @throws IllegalArgumentException
	 */
	public CustomNumberEditor(Class<?> numberClass, NumberFormat numberFormat, boolean allowEmpty) 
		throws IllegalArgumentException {
		if (numberClass == null || !Number.class.isAssignableFrom(numberClass)) {
			throw new IllegalArgumentException("Property class must be a subclass of Number");
		}
		this.numberClass = numberClass;
		this.numberFormat = numberFormat;
		this.allowEmpty = allowEmpty;
	}

	@Override
	public void setValue(Object value) {
		if (value instanceof Number) {
			super.setValue(NumberUtils.convertNumberToTargetClass((Number)value, numberClass));
		} 
		else {
			super.setValue(value);
		}
	}

	@Override
	public String getAsText() {
		Object value = getValue();
		if (value == null) {
			return "";
		}
		if (numberFormat != null) {
			return numberFormat.format(value);
		} 
		else {
			return value.toString();
		}
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		if (this.allowEmpty && !StringUtils.hasText(text)) {
			setValue(null);
			return;
		}
		if (numberFormat != null) {
			setValue(NumberUtils.parseNumber(text, numberClass, numberFormat));
			return;
		}
		setValue(NumberUtils.parseNumber(text, numberClass));
	}
	
}
