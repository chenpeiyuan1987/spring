package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;
import java.util.TimeZone;

public class TimeZoneEditor extends PropertyEditorSupport {

	@Override
	public String getAsText() {
		return null;
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		setValue(TimeZone.getTimeZone(text));
	}

}
