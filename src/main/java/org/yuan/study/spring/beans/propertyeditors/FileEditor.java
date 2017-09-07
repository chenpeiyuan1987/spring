package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;
import java.io.File;

import org.springframework.util.StringUtils;

public class FileEditor extends PropertyEditorSupport {

	@Override
	public String getAsText() {
		File value = (File) getValue();
		if (value != null) {
			return value.getAbsolutePath();
		}
		else {
			return "";
		}
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		if (StringUtils.hasText(text)) {
			setValue(new File(text));
		}
		else {
			setValue(null);
		}
	}

}
