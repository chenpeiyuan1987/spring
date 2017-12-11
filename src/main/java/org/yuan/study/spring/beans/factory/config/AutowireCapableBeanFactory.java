package org.yuan.study.spring.beans.factory.config;

import java.util.Set;

import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.TypeConverter;
import org.yuan.study.spring.beans.factory.BeanFactory;

public interface AutowireCapableBeanFactory extends BeanFactory {
	
	/** Constant that indidates no externally defined autowiring. */
	int AUTOWIRE_NO = 0;
	
	/** 
	 * Constant that indicates determining an appropriate autowire 
	 * strategy through introspection of the bean class.
	 */
	@Deprecated
	int AUTOWIRE_AUTODETECT = 1;
	
	/** Constant that indicates autowiring bean properties by name. */
	int AUTOWIRE_BY_NAME = 2;
	
	/** Constant that indicates autowiring bean properties by type. */
	int AUTOWIRE_BY_TYPE = 3;
	
	/** Constant that indicates autowiring the greediest constructor that can be satisfied. */
	int AUTOWIRE_CONSTRUCTOR = 4;
	
	//---------------------------------------------------------------------------------------
	// Typical methods for creating and populating external bean instances.
	//---------------------------------------------------------------------------------------
	
	/**
	 * Fully create a new bean instance of the given class.
	 * @param beanClass
	 * @return
	 * @throws BeansException
	 */
	<T> T createBean(Class<T> beanClass) throws BeansException;
	
	/**
	 * Populate the given bean instance through applying after-instantiation callbacks
	 * and bean property post-processing.
	 * @param existingBean
	 * @throws BeansException
	 */
	void autowireBean(Object existingBean) throws BeansException;
	
	/**
	 * Configure the given raw bean: autowiring bean properties, applying
	 * bean property values, applying factory callbacks such as 'setBeanName'
	 * and 'setBeanFactory', and also applying all bean post processors.
	 * @param existingBean
	 * @param beanName
	 * @return
	 * @throws BeansException
	 */
	Object configureBean(Object existingBean, String beanName) throws BeansException;
	
	/**
	 * Resolve the specified dependency against the beans defined in this factory.
	 * @param descriptor
	 * @param beanName
	 * @return
	 * @throws BeansException
	 */
	Object resolveDependency(DependencyDescriptor descriptor, String beanName) throws BeansException;
	
	//---------------------------------------------------------------------------------------
	// Specialized methods for fine-grained control over the bean lifecycle.
	//---------------------------------------------------------------------------------------
	
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
	
	/**
	 * Fully create a new bean instance of the given class with the specified
	 * autowire strategy. All constants defined in this interface are supported here.
	 * @param beanClass
	 * @param autowireMode
	 * @param dependencyCheck
	 * @return
	 * @throws BeansException
	 */
	Object createBean(Class<?> beanClass, int autowireMode, boolean dependencyCheck) throws BeansException;
	
	/**
	 * Initialize the given raw bean, applying factory callbacks
	 * such as 'setBeanName' and 'setBeanFactory', also applying
	 * all bean post processors.
	 * @param existingBean
	 * @param beanName
	 * @return
	 * @throws BeanException
	 */
	Object initializeBean(Object existingBean, String beanName) throws BeansException;
	
	/**
	 * Resolve the specified dependency against the beans defined in this factory.
	 * @param descriptor
	 * @param beanName
	 * @param autowiredBeanNames
	 * @param typeConverter
	 * @return
	 * @throws BeansException
	 */
	Object resolveDependency(DependencyDescriptor descriptor, String beanName, 
		Set<String> autowiredBeanNames, TypeConverter typeConverter) throws BeansException;
}
