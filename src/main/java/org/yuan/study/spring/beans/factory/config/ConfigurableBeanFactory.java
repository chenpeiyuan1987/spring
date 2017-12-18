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
	void registerCustomEditor(Class<?> requiredType, Class< ? extends PropertyEditor> propertyEditorClass);
	
	/**
	 * Set the parent of this bean factory.
	 * @param parentBeanFactory
	 */
	void setParentBeanFactory(BeanFactory parentBeanFactory) throws IllegalStateException;
	
	/**
	 * Set the class loader to use for loading bean classes.
	 * @param beanClassLoader
	 */
	void setBeanClassLoader(ClassLoader beanClassLoader);
	
	/**
	 * Return this factory's class loader for loading bean classes.
	 * @return
	 */
	ClassLoader getBeanClassLoader();
	
	/**
	 * Specify a temporary ClassLoader to use for type matching purposes, if any.
	 * @param tempClassLoader
	 */
	void setTempClassLoader(ClassLoader tempClassLoader);
	
	/**
	 * Return the temporary ClassLoader to use for type matching purposes,
	 * @return
	 */
	ClassLoader getTempClassLoader();
	
	/**
	 * Set whether to cache bean metadta such as given bean definitions
	 * (in merged fashion) and resolved bean classes.
	 * @param cacheBeanMetadata
	 */
	void setCacheBeanMetadata(boolean cacheBeanMetadata);
	
	/**
	 * Return whether to cache bean metadata such as given bean definitions
	 * (in merged fashion) and resolved bean classes.
	 * @return
	 */
	boolean isCacheBeanMetadata();
	
	/**
	 * Specify the resolution strategy for expressions in bean definition values.
	 * @param resolver
	 */
	void setBeanExpressionResolver(BeanExpressionResolver resolver);
	
	/**
	 * Return the resolution strategy for expressions in bean definition values.
	 * @return
	 */
	BeanExpressionResolver getBeanExpressionResolver();
	
	/**
	 * Specify a Spring 3.0 ConversionService to use for converting
	 * property values, as an alternative to JavaBeans PropertyEditors.
	 * @param conversionService
	 */
	void setConversionService(ConversionService conversionService);
	
	/**
	 * Return the associated ConversionService, if any.
	 * @return
	 */
	ConversionService getConversionService();
	
	/**
	 * Add a PropertyEditorRegistrar to be applied to all bean creation processes.
	 * @param registrar
	 */
	void addPropertyEditorRegistrar(PropertyEditorRegistrar registrar);
	
	/**
	 * Initialize the given PropertyEditorRegistry with the custom editors
	 * that have been registered with this BeanFactory.
	 * @param registry
	 */
	void copyRegisteredEditorsTo(PropertyEditorRegistry registry);
	
	/**
	 * Set a custom type converter that this BeanFactory should use for converting
	 * bean property values, constructor argument values, etc.
	 * @param typeConverter
	 */
	void setTypeConverter(TypeConverter typeConverter);
	
	/**
	 * Obtain a type converter as used by this BeanFactory.
	 * @return
	 */
	TypeConverter getTypeConverter();
	
	/**
	 * Add a String resolver for embedded values such as annotation attributes.
	 * @param valueResolver
	 */
	void addEmbeddedValueResolver(StringValueResolver valueResolver);
	
	/**
	 * Resolve the given embedded value, e.g. an annotation attribute.
	 * @param value
	 * @return
	 */
	String resolveEmbeddedValue(String value);
	
	/**
	 * Register the given scope, backed by the given Scope implementation.
	 * @param scopeName
	 * @param scope
	 */
	void registerScope(String scopeName, Scope scope);
	
	/**
	 * Return the names of all currently registered scopes.
	 * @return
	 */
	String[] getRegisteredScopeNames();
	
	/**
	 * Provides a security access control context relevant to this factory.
	 * @return
	 */
	AccessControlContext getAccessControlContext();
	
	/**
	 * Copy all relevant configuration from the given other factory.
	 * @param otherFactory
	 */
	void copyConfigurationFrom(ConfigurableBeanFactory otherFactory);
	
	/**
	 * Resolve all alias target names and aliases registered in this
	 * factory, applying the given StringValueResolver to them.
	 * @param valueResolver
	 */
	void resolveAliases(StringValueResolver valueResolver);
	
	/**
	 * Return a merged BeanDefinition for the given bean name,
	 * merging a child bean definition with its parent if necessary.
	 * @param beanName
	 * @return
	 * @throws NoSuchBeanDefinitionException
	 */
	BeanDefinition getMergedBeanDefinition(String beanName) throws NoSuchBeanDefinitionException;

	/**
	 * Determine whether the bean with the given name is a FacotryBean.
	 * @param name
	 * @return
	 * @throws NoSuchBeanDefinitionException
	 */
	boolean isFactoryBean(String name) throws NoSuchBeanDefinitionException;
	
	/**
	 * Determine whether the specified bean is currently in creation.
	 * @param beanName
	 * @return
	 */
	boolean isCurrentlyInCreation(String beanName);
	
	/**
	 * Register a dependent bean for the given bean,
	 * to be destroyed before the given bean is destroyed.
	 * @param beanName
	 * @param dependentBeanName
	 */
	void registerDependentBean(String beanName, String dependentBeanName);
	
	/**
	 * Return the names of all beans which depend on the specified bean, if any.
	 * @param beanName
	 * @return
	 */
	String[] getDependentBeans(String beanName);
	
	/**
	 * Return the names of all beans that the specified bean depends on, if any.
	 * @param beanName
	 * @return
	 */
	String[] getDependenciesForBean(String beanName);
	
	/**
	 * Destroy the specified scoped bean in the current target scope, if any.
	 * @param beanName
	 */
	void destroyScopedBean(String beanName);
	
	/**
	 * Destroy the given bean instance (usually a prototype instance
	 * obtained from this factory) according to its bean definition.
	 * @param beanName
	 * @param beanInstance
	 */
	void destroyBean(String beanName, Object beanInstance);
}
