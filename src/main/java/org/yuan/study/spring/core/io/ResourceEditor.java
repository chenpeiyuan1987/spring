package org.yuan.study.spring.core.io;

import java.beans.PropertyEditorSupport;

import org.yuan.study.spring.util.StringUtils;
import org.yuan.study.spring.util.SystemPropertyUtils;

public class ResourceEditor extends PropertyEditorSupport {

	private final ResourceLoader resourceLoader;
	
	/**
	 * Create a new ResourceEditor with the given ResourceLoader.
	 */
	public ResourceEditor() {
		this.resourceLoader = new DefaultResourceLoader();
	}

	/**
	 * Create a new ResourceEditor with the given ResourceLoader.
	 * @param resourceLoader
	 */
	public ResourceEditor(ResourceLoader resourceLoader) {
		this.resourceLoader = resourceLoader;
	}

	/**
	 * Resolve the given path, replacing placeholders with 
	 * corresponding system property values if necessary.
	 * @param path
	 * @return
	 */
	protected String resolvePath(String path) {
		return SystemPropertyUtils.resolvePlaceholders(path);
	}

	@Override
	public void setAsText(String text) throws IllegalArgumentException {
		if (StringUtils.hasText(text)) {
			String locationToUse = resolvePath(text).trim();
			setValue(this.resourceLoader.getResource(locationToUse));
		} else {
			setValue(null);
		}
	}
	
}
