package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;
import java.nio.charset.Charset;

import org.yuan.study.spring.util.StringUtils;

public class CharsetEditor extends PropertyEditorSupport {

	@Override
	public String getAsText() {
		Charset value = (Charset) getValue();
		return (value != null ? value.name() : "");
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		if (StringUtils.hasText(text)) {
			setValue(Charset.forName(text));
		}
		else {
			setValue(null);
		}
	}

}
