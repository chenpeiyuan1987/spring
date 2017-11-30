package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

public class PropertiesEditor extends PropertyEditorSupport {

	public final static String COMMENT_MARKERS = "#!";
	
	/**
	 * Remove comment lines, even if they contain whitespace before the comment marker.
	 * This happens automatically on JDK >= 1.4, but we need to do this manually on JDK 1.3.
	 * @param props
	 */
	private void dropComments(Properties props) {
		Iterator<Object> keys = props.keySet().iterator();
		while (keys.hasNext()) {
			String key = (String) keys.next();
			if (key.length() > 0 && COMMENT_MARKERS.indexOf(key.charAt(0)) != -1) {
				keys.remove();
			}
		}
	}

	@Override
	public void setValue(Object value) {
		if (!(value instanceof Properties) && value instanceof Map) {
			Properties props = new Properties();
			props.putAll((Map) value);
			super.setValue(props);
		}
		else {
			super.setValue(value);
		}
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		Properties props = new Properties();
		if (text != null) {
			try {
				props.load(new ByteArrayInputStream(text.getBytes("ISO-8859-1")));
			}
			catch (IOException ex) {
				throw new IllegalArgumentException(String.format("Failed to parse [%s] into Properties", text));
			}
		}
		setValue(props);
	}
	
}
