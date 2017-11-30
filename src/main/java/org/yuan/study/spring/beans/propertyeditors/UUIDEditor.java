package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;
import java.util.UUID;

import org.yuan.study.spring.util.StringUtils;

public class UUIDEditor extends PropertyEditorSupport {

	@Override
	public String getAsText() {
		UUID value = (UUID) getValue();
		return (value != null ? value.toString() : "");
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		if (StringUtils.hasText(text)) {
			setValue(UUID.fromString(text));
		}
		else {
			setValue(null);
		}
	}

}
