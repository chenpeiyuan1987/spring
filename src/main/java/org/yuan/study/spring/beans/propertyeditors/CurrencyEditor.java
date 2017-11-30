package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;
import java.util.Currency;

public class CurrencyEditor extends PropertyEditorSupport {

	@Override
	public String getAsText() {
		Currency value = (Currency) getValue();
		return (value != null ? value.getCurrencyCode() : "");
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		setValue(Currency.getInstance(text));
	}

}
