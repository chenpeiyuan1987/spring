package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;
import java.io.IOException;

import org.xml.sax.InputSource;
import org.yuan.study.spring.core.io.Resource;
import org.yuan.study.spring.core.io.ResourceEditor;
import org.yuan.study.spring.util.Assert;

public class InputSourceEditor extends PropertyEditorSupport {

	private final ResourceEditor resoruceEditor;

	public InputSourceEditor() {
		this(new ResourceEditor());
	}

	public InputSourceEditor(ResourceEditor resourceEditor) {
		Assert.notNull(resourceEditor, "ResourceEditor must not be null");
		
		this.resoruceEditor = resourceEditor;
	}

	@Override
	public String getAsText() {
		InputSource value = (InputSource) getValue();
		return (value != null ? value.getSystemId() : "");
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		resoruceEditor.setAsText(text);
		Resource resource = (Resource) resoruceEditor.getValue();
		try {
			setValue(resource != null ? new InputSource(resource.getURL().toString()) : null);
		} 
		catch (IOException ex) {
			throw new IllegalArgumentException(String.format(
				"Could not retrieve URL for %s: %s", resource, ex.getMessage()));
		}
	}
	
}
