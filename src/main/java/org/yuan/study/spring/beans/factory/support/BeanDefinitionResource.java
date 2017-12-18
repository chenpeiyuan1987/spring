package org.yuan.study.spring.beans.factory.support;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.core.io.AbstractResource;
import org.yuan.study.spring.util.Assert;

public class BeanDefinitionResource extends AbstractResource {

	private final BeanDefinition beanDefinition;
	
	/**
	 * Create a new BeanDefinitionResource.
	 * @param beanDefinition
	 */
	public BeanDefinitionResource(BeanDefinition beanDefinition) {
		Assert.notNull(beanDefinition, "BeanDefinition must not be null");
		this.beanDefinition = beanDefinition;
	}
	
	/**
	 * Return the wrapped BeanDefinition object.
	 * @return
	 */
	public final BeanDefinition getBeanDefinition() {
		return beanDefinition;
	}
	
	@Override
	public boolean exists() {
		return false;
	}

	@Override
	public boolean isReadable() {
		return false;
	}

	@Override
	public int hashCode() {
		return beanDefinition.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == this) {
			return true;
		}
		if (obj instanceof BeanDefinitionResource) {
			return ((BeanDefinitionResource) obj).beanDefinition.equals(beanDefinition);
		}
		return false;
	}

	@Override
	public String getDescription() {
		return "BeanDefinition defined in " + beanDefinition.getResourceDescription();
	}

	@Override
	public InputStream getInputStream() throws IOException {
		throw new FileNotFoundException(
			"Resource cannot be opened because it points to " + getDescription());
	}

}
