package org.yuan.study.spring.beans.factory;

import java.lang.annotation.Annotation;
import java.util.Map;

import org.yuan.study.spring.beans.BeansException;

public interface ListableBeanFactory extends BeanFactory {
	/**
	 * Check if this bean factory contains a bean definition with the given name.
	 * @param beanName
	 * @return
	 */
	boolean containsBeanDefinition(String name);
	
	/**
	 * Return the number of beans defined in the factory.
	 * @return
	 */
	int getBeanDefinitionCount();
	
	/**
	 * Return the names of all beans defined in this factory.
	 * @return
	 */
	String[] getBeanDefinitionNames();
	
	/**
	 * Return the names of beans matching the given type (including subclasses),
	 * judging from either bean definitions or the value of getObjectType in the case
	 * of FactoryBeans.
	 * @param Type
	 * @return
	 */
	String[] getBeanNamesForType(Class<?> type);
	
	/**
	 * Return the names of beans matching the given type (including subclasses),
	 * judging from either bean definitions or the value of getObjectType in the case
	 * of FactoryBeans.
	 * @param type
	 * @param includePrototypes
	 * @param includeFactoryBeans
	 * @return
	 */
	String[] getBeanNamesForType(Class<?> type, boolean includeNonSingletons, boolean allowEagerInit);
	
	/**
	 * Return the bean instances that match the given object type (including subclasses), 
	 * judging from either bean definitions or the value of getObjectType in the case of FactoryBeans.
	 * @param type
	 * @return
	 */
	<T> Map<String, T> getBeansOfType(Class<T> type) throws BeansException;
	
	/**
	 * Return the bean instances that match the given object type (including subclasses), 
	 * judging from either bean definitions or the value of getObjectType in the case of FactoryBeans.
	 * @param type
	 * @param includeNonSingletons
	 * @param allowEagerInit
	 * @return
	 * @throws BeansException
	 */
	<T> Map<String, T> getBeansOfType(Class<T> type, boolean includeNonSingletons, boolean allowEagerInit) throws BeansException;

	/**
	 * Find all beans whose Class has the supplied 'java.lang.annotation.Annotation' type.
	 * @param annotationType
	 * @return
	 * @throws BeansException
	 */
	Map<String, Object> getBeansWithAnnotation(Class<? extends Annotation> annotationType) throws BeansException;
	
	/**
	 * Find a 'Annotation' of annotationType on the specified bean, traversing its interfaces and 
	 * super classes if no annotation can be found on the given class itself.
	 * @param beanName
	 * @param annotationType
	 * @return
	 */
	<T extends Annotation> T findAnnotationOnBean(String beanName, Class<T> annotationType); 
}
