package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;

import org.yuan.study.spring.util.StringUtils;

public class CharacterEditor extends PropertyEditorSupport {
	
	private static final String UNICODE_PREFIX = "\\u";
	
	private static final int UNICODE_LENGTH = 6;

	private final boolean allowEmpty;
	
	public CharacterEditor(boolean allowEmpty) {
		this.allowEmpty = allowEmpty;
	}

	@Override
	public String getAsText() {
		Object value = getValue();
		return (value != null ? value.toString() : "");
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		if (allowEmpty && !StringUtils.hasLength(text)) {
			setValue(null);
		} 
		else if (text == null) {
			throw new IllegalArgumentException("null String cannot be converted to char type");
		}
		else if (isUnicodeCharacterSequence(text)) {
			setAsUnicode(text);
		}
		else if (text.length() != 1) {
			throw new IllegalArgumentException(
				String.format("String [%s] with length cannot be converted to char type", text, text.length()));
		}
		else {
			setValue(new Character(text.charAt(0)));
		}
	}

	private boolean isUnicodeCharacterSequence(String sequence) {
		return (sequence.startsWith(UNICODE_PREFIX) && sequence.length() == UNICODE_LENGTH);
	}
	
	private void setAsUnicode(String text) {
		int code = Integer.parseInt(text.substring(UNICODE_PREFIX.length()), 16);
		setValue(new Character((char) code));
	}
}
