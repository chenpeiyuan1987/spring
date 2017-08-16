package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;

public class ByteArrayPropertyEditor extends PropertyEditorSupport {

	@Override
	public String getAsText() {
		byte[] value = (byte[]) getValue();
		return (value != null ? new String(value) : "");
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		setValue(text != null ? text.getBytes() : null);
	}

}
