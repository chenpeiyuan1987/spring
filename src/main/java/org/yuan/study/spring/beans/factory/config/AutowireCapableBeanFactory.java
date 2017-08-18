package org.yuan.study.spring.beans.factory.config;

import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.factory.BeanFactory;

public interface AutowireCapableBeanFactory extends BeanFactory {
	
	/** Constant that indicates determining an appropriate autowire strategy through introspection of the bean class. */
	int AUTOWIRE_AUTODETECT = 1;
	/** Constant that indicates autowiring bean properties by name. */
	int AUTOWIRE_BY_NAME = 2;
	/** Constant that indicates autowiring bean properties by type. */
	int AUTOWIRE_BY_TYPE = 3;
	/** Constant that indicates autowiring a constructor. */
	int AUTOWIRE_CONSTRUCTOR = 4;
	
	/**
	 * Apply BeanPostProcessors to the given existing bean instance.
	 * invoking their postProcessAfterInitialization methods.
	 * The returned bean instance may be a wrapper around the original.
	 * @param existingBean
	 * @param beanName
	 * @return
	 * @throws BeansException
	 */
	Object applyBeanPostProcessorsAfterInitialization(Object existingBean, String beanName) throws BeansException;
	
	/**
	 * Apply BeanPostProcessors to the given existing bean instance.
	 * invoking their postProcessBeforeInitialization methods.
	 * The returned bean instance may be a wrapper around the original.
	 * @param existingBean
	 * @param beanName
	 * @return
	 * @throws BeansException
	 */
	Object applyBeanPostProcessorsBeforeInitialization(Object existingBean, String beanName) throws BeansException;

	/**
	 * Apply the property values of the bean definition with the given name to the given bean instance.
	 * @param existingBean
	 * @param beanName
	 * @throws BeansException
	 */
	void applyBeanPropertyValues(Object existingBean, String beanName) throws BeansException;
	
	/**
	 * Create a new bean instance of the given class with the specified autowire strategy.
	 * @param beanClass
	 * @param autowireMode
	 * @param dependencyCheck
	 * @return
	 * @throws BeansException
	 */
	Object autowire(Class<?> beanClass, int autowireMode, boolean dependencyCheck) throws BeansException;
	
	/**
	 * Autowire the bean properties of the given bean instance by name or type.
	 * @param existingBean
	 * @param autowireMode
	 * @param dependencyCheck
	 * @throws BeansException
	 */
	void autowireBeanProperties(Object existingBean, int autowireMode, boolean dependencyCheck) throws BeansException;
}
