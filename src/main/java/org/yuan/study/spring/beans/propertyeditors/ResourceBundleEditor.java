package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;
import java.util.Locale;
import java.util.ResourceBundle;

import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.StringUtils;

public class ResourceBundleEditor extends PropertyEditorSupport {

	public static final String BASE_NAME_SEPARATOR = "_";

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		Assert.hasText(text, "Text must not be empty");
		
		ResourceBundle bundle;
		String rawBaseName = text.trim();
		int indexOfBaseNameSeparator = rawBaseName.indexOf(BASE_NAME_SEPARATOR);
		if (indexOfBaseNameSeparator == -1) {
			bundle = ResourceBundle.getBundle(rawBaseName);
		} 
		else {
			String baseName = rawBaseName.substring(0, indexOfBaseNameSeparator);
			if (!StringUtils.hasText(baseName)) {
				throw new IllegalArgumentException(String.format(
					"Bad ResourceBundle name : received '%s' as argument to 'setAsText(String value)'.", text));
			}
			String localeString = rawBaseName.substring(indexOfBaseNameSeparator + 1);
			Locale locale = StringUtils.parseLocaleString(localeString);
			bundle = (StringUtils.hasText(localeString)) 
				? ResourceBundle.getBundle(baseName, locale)
				: ResourceBundle.getBundle(baseName);
		}
		setValue(bundle);
	}
	
}
