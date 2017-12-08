package org.yuan.study.spring.beans.factory.config;

import java.beans.PropertyEditor;
import java.security.AccessControlContext;

import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.PropertyEditorRegistrar;
import org.yuan.study.spring.beans.PropertyEditorRegistry;
import org.yuan.study.spring.beans.TypeConverter;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.HierarchicalBeanFactory;
import org.yuan.study.spring.beans.factory.NoSuchBeanDefinitionException;
import org.yuan.study.spring.core.convert.ConversionService;
import org.yuan.study.spring.util.StringValueResolver;

public interface ConfigurableBeanFactory extends HierarchicalBeanFactory, SingletonBeanRegistry {

	/**
	 * Scope identifier for the standard singleton scope: "singleton".
	 */
	String SCOPE_SINGLETON = "singleton";
	
	/**
	 * Scope identifier for the standard prototype scope: "prototype".
	 */
	String SCOPE_PROTOTYPE = "prototype";
	
	/**
	 * Set the parent of this bean factory.
	 * @param beanPostProcessor
	 */
	void addBeanPostProcessor(BeanPostProcessor beanPostProcessor);
	
	/**
	 * Destroy all cached singletons in this factory.
	 * @param beanName
	 */
	void destroySingletons();
	
	/**
	 * Return the current number of registered BeanPostProcessors.
	 * @return
	 */
	int getBeanPostProcessorCount();
	
	/**
	 * Given a bean name, create an alias. 
	 * We typically use this method to support names that are illegal within XML ids.
	 * @param beanName
	 * @param alias
	 */
	void registerAlias(String beanName, String alias) throws BeansException;
	
	/**
	 * Register the given custom property editor for all properties of the given type.
	 * @param requiredType
	 * @param propertyEditor
	 */
	void registerCustomEditor(Class<?> requiredType, PropertyEditor propertyEditor);
	
	/**
	 * Set the parent of this bean factory.
	 * @param parentBeanFactory
	 */
	void setParentBeanFactory(BeanFactory parentBeanFactory) throws IllegalStateException;
	
	/**
	 * 
	 * @param beanClassLoader
	 */
	void setBeanClassLoader(ClassLoader beanClassLoader);
	
	ClassLoader getBeanClassLoader();
	
	void setTempClassLoader(ClassLoader tempClassLoader);
	
	ClassLoader getTempClassLoader();
	
	void setCacheBeanMetadata(boolean cacheBeanMetadata);
	
	boolean isCacheBeanMetadata();
	
	void setBeanExpressionResolver(BeanExpressionResolver resolver);
	
	BeanExpressionResolver getBeanExpressionResolver();
	
	void setConversionService(ConversionService conversionService);
	
	ConversionService getConversionService();
	
	void addPropertyEditorRegistrar(PropertyEditorRegistrar registrar);
	
	void copyRegisteredEditorsTo(PropertyEditorRegistry registry);
	
	void setTypeConverter(TypeConverter typeConverter);
	
	TypeConverter getTypeConverter();
	
	void addEmbeddedValueResolver(StringValueResolver valueResolver);
	
	String resolveEmbeddedValue(String value);
	
	void registerScope(String scopeName, Scope scope);
	
	String[] getRegisteredScopeNames();
	
	AccessControlContext getAccessControlContext();
	
	void copyConfigurationFrom(ConfigurableBeanFactory otherFactory);
	
	void resolveAliases(StringValueResolver valueResolver);
	
	BeanDefinition getMergedBeanDefinition(String beanName) throws NoSuchBeanDefinitionException;

	boolean isFactoryBean(String name) throws NoSuchBeanDefinitionException;
	
	boolean isCurrentlyInCreation(String beanName);
	
	void registerDependentBean(String beanName, String dependentBeanName);
	
	String[] getDependentBeans(String beanName);
	
	String[] getDependenciesForBean(String beanName);
	
	void destroyScopedBean(String beanName);
	
	void destroyBean(String beanName, Object beanInstance);
}
