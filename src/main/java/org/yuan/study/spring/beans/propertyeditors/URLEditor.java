package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;
import java.io.IOException;
import java.net.URL;

import org.yuan.study.spring.core.io.Resource;
import org.yuan.study.spring.core.io.ResourceEditor;

public class URLEditor extends PropertyEditorSupport {

	private final ResourceEditor resourceEditor;
	
	/**
	 * Create a new URLEditor, using the default ResourceEditor underneath.
	 */
	public URLEditor() {
		this.resourceEditor = new ResourceEditor();
	}

	/**
	 * Create a new URLEditor, using the given ResourceEditor underneath.
	 * @param resourceEditor
	 */
	public URLEditor(ResourceEditor resourceEditor) {
		this.resourceEditor = resourceEditor;
	}

	@Override
	public String getAsText() {
		URL value = (URL) getValue();
		return (value != null ? value.toExternalForm() : "");
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		this.resourceEditor.setAsText(text);
		Resource resource = (Resource) this.resourceEditor.getValue();
		try {
			setValue(resource != null ? resource.getURL() : null);
		}
		catch (IOException ex) {
			throw new IllegalArgumentException(String.format("Could not retrieve URL for %s: %s", resource, ex.getMessage()));
		}
	}
	
}
