package org.yuan.study.spring.beans.propertyeditors;

import java.beans.PropertyEditorSupport;
import java.io.File;
import java.io.IOException;

import org.yuan.study.spring.core.io.Resource;
import org.yuan.study.spring.core.io.ResourceEditor;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ResourceUtils;
import org.yuan.study.spring.util.StringUtils;

public class FileEditor extends PropertyEditorSupport {
	
	private final ResourceEditor resourceEditor;
	
	public FileEditor() {
		resourceEditor = new ResourceEditor();
	}
	
	public FileEditor(ResourceEditor resourceEditor) {
		Assert.notNull(resourceEditor, "ResourceEditor must not be null");
		
		this.resourceEditor = resourceEditor;
	}

	@Override
	public String getAsText() {
		File value = (File) getValue();
		if (value != null) {
			return value.getPath();
		}
		else {
			return "";
		}
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		if (!StringUtils.hasText(text)) {
			setValue(null);
			return;
		}
		
		if (!ResourceUtils.isUrl(text)) {
			File file = new File(text);
			if (file.isAbsolute()) {
				setValue(file);
				return;
			}
		}
		
		resourceEditor.setAsText(text);
		Resource resource = (Resource) resourceEditor.getValue();
		if (ResourceUtils.isUrl(text) || resource.exists()) {
			try {
				setValue(resource.getFile());
			} 
			catch (IOException ex) {
				throw new IllegalArgumentException(String.format("Could not retrieve File for %s: %s", resource, ex.getMessage()));
			}
		} 
		else {
			setValue(new File(text));
		}
	}

}
