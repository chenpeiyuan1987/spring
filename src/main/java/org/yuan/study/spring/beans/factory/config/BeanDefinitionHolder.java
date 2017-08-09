package org.yuan.study.spring.beans.factory.config;


public class BeanDefinitionHolder {

	private final BeanDefinition beanDefinition;
	
	private final String beanName;
	
	private final String[] aliases;

	/**
	 * Create a new BeanDefinitionHolder.
	 * @param beanDefinition
	 * @param beanName
	 */
	public BeanDefinitionHolder(BeanDefinition beanDefinition, String beanName) {
		this(beanDefinition, beanName, null);
	}
	
	/**
	 * Create a new BeanDefinitionHolder.
	 * @param beanDefinition
	 * @param beanName
	 * @param aliases
	 */
	public BeanDefinitionHolder(BeanDefinition beanDefinition, String beanName, String[] aliases) {
		this.beanDefinition = beanDefinition;
		this.beanName = beanName;
		this.aliases = aliases;
	}

	/**
	 * Return the wrapped BeanDefinition
	 * @return
	 */
	public BeanDefinition getBeanDefinition() {
		return beanDefinition;
	}

	/**
	 * Return the primary name of the bean, as specified for the bean definition.
	 * @return
	 */
	public String getBeanName() {
		return beanName;
	}

	/**
	 * Return the alias names for the bean, as specified directly for the bean definition.
	 * @return
	 */
	public String[] getAliases() {
		return aliases;
	}

	//---------------------------------------------------------
	// Implementation other methods
	//---------------------------------------------------------
	
	@Override
	public String toString() {
		return String.format("Bean definition with name '%s': %s", beanName, beanDefinition);
	}
	
}
