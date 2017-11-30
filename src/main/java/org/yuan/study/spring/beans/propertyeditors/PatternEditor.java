package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;
import java.util.regex.Pattern;

public class PatternEditor extends PropertyEditorSupport {

	private final int flags;

	public PatternEditor() {
		this(0);
	}
	
	public PatternEditor(int flags) {
		this.flags = flags;
	}

	@Override
	public String getAsText() {
		Pattern value = (Pattern) getValue();
		return (value != null ? value.pattern() : "");
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		setValue(text != null ? Pattern.compile(text, flags) : null);
	}
	
}
