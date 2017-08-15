package org.yuan.study.spring.beans.factory.support;

import org.yuan.study.spring.beans.factory.config.BeanDefinitionHolder;

public abstract class BeanDefinitionReaderUtils {
	
	/**
	 * Register the given bean definition with the given bean factory.
	 * @param holder
	 * @param registry
	 */
	public static void registerBeanDefinition(BeanDefinitionHolder holder, BeanDefinitionRegistry registry) {
		registry.registerBeanDefinition(holder.getBeanName(), holder.getBeanDefinition());
		
		if (holder.getAliases() != null) {
			for (String alias : holder.getAliases()) {
				registry.registerAlias(holder.getBeanName(), alias);
			}
		}
	}
}
