package org.yuan.study.spring.beans.factory.config;

import org.yuan.study.spring.beans.BeanMetadataElement;

public interface BeanReference extends BeanMetadataElement {

	/**
	 * Return the target bean name that this reference points to.
	 * @return
	 */
	String getBeanName();
}
