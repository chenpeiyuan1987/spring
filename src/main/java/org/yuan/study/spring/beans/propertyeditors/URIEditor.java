package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import org.yuan.study.spring.core.io.ClassPathResource;
import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.ResourceUtils;
import org.yuan.study.spring.util.StringUtils;

public class URIEditor extends PropertyEditorSupport {

	private final ClassLoader classLoader;
	
	private final boolean encode;

	public URIEditor() {
		this(null, true);
	}

	public URIEditor(ClassLoader classLoader) {
		this(classLoader, true);
	}

	public URIEditor(boolean encode) {
		this(null, encode);
	}

	public URIEditor(ClassLoader classLoader, boolean encode) {
		this.classLoader = classLoader != null ? classLoader : ClassUtils.getDefaultClassLoader();
		this.encode = encode;
	}

	@Override
	public String getAsText() {
		URI value = (URI) getValue();
		return (value != null ? value.toString() : "");
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		if (StringUtils.hasText(text)) {
			String uri = text.trim();
			if (classLoader != null && uri.startsWith(ResourceUtils.CLASSPATH_URL_PREFIX)) {
				ClassPathResource resource = new ClassPathResource(uri.substring(ResourceUtils.CLASSPATH_URL_PREFIX.length()), classLoader);
				try {
					String url = resource.getURL().toString();
					setValue(createURI(url));
				} 
				catch (IOException ex) {
					throw new IllegalArgumentException(String.format("Could not retrieve URI for %s: %s", resource, ex.getMessage()));
				}
				catch (URISyntaxException ex) {
					throw new IllegalArgumentException("Invalid URI syntax: " + ex);
				}
			}
			else {
				try {
					setValue(createURI(uri));
				}
				catch (URISyntaxException ex) {
					throw new IllegalArgumentException("Invalid URI syntax: " + ex);
				}
			}
		} 
		else {
			setValue(null);
		}
	}
	
	protected URI createURI(String value) throws URISyntaxException {
		int colonIndex = value.indexOf(":");
		if (encode && colonIndex != -1) {
			int fragmentIndex = value.indexOf('#', colonIndex + 1);
			String schema = value.substring(0, colonIndex);
			String ssp = value.substring(colonIndex + 1, (fragmentIndex > 0 ? fragmentIndex : value.length()));
			String fragment = (fragmentIndex > 0 ? value.substring(fragmentIndex + 1) : null);
			return new URI(schema, ssp, fragment);
		} 
		else {
			return new URI(value);
		}
	}
}
