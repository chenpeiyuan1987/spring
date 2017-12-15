package org.yuan.study.spring.beans.factory.support;

import java.beans.PropertyEditor;
import java.security.AccessControlContext;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.yuan.study.spring.beans.BeanUtils;
import org.yuan.study.spring.beans.BeanWrapper;
import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.PropertyEditorRegistrar;
import org.yuan.study.spring.beans.PropertyEditorRegistry;
import org.yuan.study.spring.beans.PropertyEditorRegistrySupport;
import org.yuan.study.spring.beans.SimpleTypeConverter;
import org.yuan.study.spring.beans.TypeConverter;
import org.yuan.study.spring.beans.TypeMismatchException;
import org.yuan.study.spring.beans.factory.BeanCreationException;
import org.yuan.study.spring.beans.factory.BeanCurrentlyInCreationException;
import org.yuan.study.spring.beans.factory.BeanDefinitionStoreException;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.BeanFactoryUtils;
import org.yuan.study.spring.beans.factory.BeanIsAbstractException;
import org.yuan.study.spring.beans.factory.BeanIsNotAFactoryException;
import org.yuan.study.spring.beans.factory.BeanNotOfRequiredTypeException;
import org.yuan.study.spring.beans.factory.CannotLoadBeanClassException;
import org.yuan.study.spring.beans.factory.FactoryBean;
import org.yuan.study.spring.beans.factory.NoSuchBeanDefinitionException;
import org.yuan.study.spring.beans.factory.ObjectFactory;
import org.yuan.study.spring.beans.factory.SmartFactoryBean;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.beans.factory.config.BeanDefinitionHolder;
import org.yuan.study.spring.beans.factory.config.BeanExpressionContext;
import org.yuan.study.spring.beans.factory.config.BeanExpressionResolver;
import org.yuan.study.spring.beans.factory.config.BeanPostProcessor;
import org.yuan.study.spring.beans.factory.config.ConfigurableBeanFactory;
import org.yuan.study.spring.beans.factory.config.DestructionAwareBeanPostProcessor;
import org.yuan.study.spring.beans.factory.config.InstantiationAwareBeanPostProcessor;
import org.yuan.study.spring.beans.factory.config.Scope;
import org.yuan.study.spring.core.DecoratingClassLoader;
import org.yuan.study.spring.core.NamedThreadLocal;
import org.yuan.study.spring.core.convert.ConversionService;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.ObjectUtils;
import org.yuan.study.spring.util.StringUtils;
import org.yuan.study.spring.util.StringValueResolver;

public abstract class AbstractBeanFactory extends FactoryBeanRegistrySupport implements ConfigurableBeanFactory {
	
	/** Parent bean factory, for bean inheritance support */
	private BeanFactory parentBeanFactory;
	
	/** ClassLoader to resolve bean class names with, if necessary */
	private ClassLoader beanClassLoader = ClassUtils.getDefaultClassLoader();
	
	/** ClassLoader to temporarily resolve bean class names with, if necessary */
	private ClassLoader tempClassLoader;
	
	/** Whether to cache bean metadata or rather reobtain it for every access */
	private boolean cacheBeanMetadata = true;
	
	/** Resolution strategy for expressions in bean definition values */
	private BeanExpressionResolver beanExpressionResolver;
	
	/** Spring 3.0 ConversionService to use instead of PropertyEditors */
	private ConversionService conversionService;
	
	/** Custom PropertyEditorRegistrars to apply to the beans of this factory */
	private final Set<PropertyEditorRegistrar> propertyEditorRegistrars = new LinkedHashSet<PropertyEditorRegistrar>();
	
	/** A custom TypeConverter to use, overriding the default PropertyEditor mechanism */
	private TypeConverter typeConverter;
	
	/** Custom PropertyEditors to apply to the beans of this factory */
	private final Map<Class<?>, Class<? extends PropertyEditor>> customEditors = 
		new HashMap<Class<?>, Class<? extends PropertyEditor>>(4);
	
	/** String resolvers to apply e.g. to annotation attribute values */
	private final List<StringValueResolver> embeddedValueResolvers = new LinkedList<StringValueResolver>();
	
	/** BeanPostProcessors to apply in createBean */
	private final List<BeanPostProcessor> beanPostProcessors = new ArrayList<BeanPostProcessor>();
	
	/** Indicates whether any InstantiationAwareBeanPostProcessors have been registered */
	private boolean hasInstantiationAwareBeanPostProcessors;
	
	/** Indicates whether any DestructionAwareBeanPostProcessors have been registered */
	private boolean hasDestructionAwareBeanPostProcessors;
	
	/** Map from scope identifier String to corresponding Scope */
	private final Map<String, Scope> scopes = new HashMap<String, Scope>();
	
	/** Security context used when running with a SecurityManager */
	private SecurityContextProvider securityContextProvider;
	
	/** Map from bean name to merged RootBeanDefinition */
	private final Map<String, RootBeanDefinition> mergedBeanDefinitions = new ConcurrentHashMap<String, RootBeanDefinition>();
	
	/** Names of beans that have already bean created at least once */
	private final Set<String> alreadyCreated = Collections.synchronizedSet(new HashSet<String>());
	
	/** Names of beans that are currently in creation */
	private final ThreadLocal<Object> prototypesCurrentlyInCreation = new NamedThreadLocal<Object>("Prototype beans currently in creation");
	
	/**
	 * Create a new AbstractBeanFactory.
	 */
	public AbstractBeanFactory() {
	}

	/**
	 * Create a new AbstractBeanFactory with the given parent.
	 * @param parentBeanFactory
	 */
	public AbstractBeanFactory(BeanFactory parentBeanFactory) {
		this.parentBeanFactory = parentBeanFactory;
	}
	
	//-------------------------------------------------------------------
	// Implementation of BeanFactory interface
	//-------------------------------------------------------------------

	@Override
	public Object getBean(String name) throws BeansException {
		return doGetBean(name, null, null, false);
	}

	@Override
	public <T> T getBean(String name, Class<T> requiredType) throws BeansException {
		return doGetBean(name, requiredType, null, false);
	}

	@Override
	public Object getBean(String name, Object... args) throws BeansException {
		return doGetBean(name, null, args, false);
	}

	@Override
	public Class<?> getType(String name) throws NoSuchBeanDefinitionException {
		String beanName = transformedBeanName(name);
		
		Object beanInstance = getSingleton(beanName, false);
		if (beanInstance != null) {
			if (beanInstance instanceof FactoryBean && !BeanFactoryUtils.isFactoryDereference(name)) {
				return getTypeForFactoryBean((FactoryBean<?>) beanInstance);
			} 
			else {
				return beanInstance.getClass();
			}
		}
		else if (containsSingleton(beanName) && !containsBeanDefinition(beanName)) {
			return null;
		}
		else {
			BeanFactory parentBeanFactory = getParentBeanFactory();
			if (parentBeanFactory != null && !containsBeanDefinition(beanName)) {
				return parentBeanFactory.getType(originalBeanName(name));
			}
			
			RootBeanDefinition mbd = getMergedLocalBeanDefinition(beanName);
			
			BeanDefinitionHolder dbd = mbd.getDecoratedDefinition();
			if (dbd != null && !BeanFactoryUtils.isFactoryDereference(name)) {
				RootBeanDefinition tbd = getMergedBeanDefinition(dbd.getBeanName(), dbd.getBeanDefinition(), mbd);
				Class<?> targetClass = predictBeanType(dbd.getBeanName(), tbd);
				if (targetClass != null && !FactoryBean.class.isAssignableFrom(targetClass)) {
					return targetClass;
				}
			}
			
			Class<?> beanClass = predictBeanType(beanName, mbd);
			
			if (beanClass != null && FactoryBean.class.isAssignableFrom(beanClass)) {
				if (!BeanFactoryUtils.isFactoryDereference(name)) {
					return getTypeForFactoryBean(beanName, mbd);
				} 
				else {
					return beanClass;
				}
			} 
			else {
				return (!BeanFactoryUtils.isFactoryDereference(name) ? beanClass : null);
			}
		}
	}

	@Override
	public boolean isSingleton(String name) throws NoSuchBeanDefinitionException {
		String beanName = transformedBeanName(name);
		
		Object beanInstance = getSingleton(beanName, false);
		if (beanInstance != null) {
			if (beanInstance instanceof FactoryBean) {
				return (BeanFactoryUtils.isFactoryDereference(name) || ((FactoryBean<?>) beanInstance).isSingleton());
			} 
			else {
				return !BeanFactoryUtils.isFactoryDereference(name);
			}
		} 
		else if (containsSingleton(beanName)) {
			return true;
		}
		else {
			BeanFactory parentBeanFactory = getParentBeanFactory();
			if (parentBeanFactory != null && !containsBeanDefinition(beanName)) {
				return parentBeanFactory.isSingleton(originalBeanName(name));
			}
			
			RootBeanDefinition mbd = getMergedLocalBeanDefinition(beanName);
			
			if (mbd.isSingleton()) {
				if (isFactoryBean(beanName, mbd)) {
					if (BeanFactoryUtils.isFactoryDereference(name)) {
						return true;
					}
					FactoryBean<?> factoryBean = (FactoryBean<?>) getBean(FACTORY_BEAN_PREFIX + beanName);
					return factoryBean.isSingleton();
				} 
				else {
					return !BeanFactoryUtils.isFactoryDereference(name);
				}
			}
			else {
				return false;
			}
		}
	}

	@Override
	public boolean containsBean(String name) {
		String beanName = transformedBeanName(name);
		if (containsSingleton(beanName) || containsBeanDefinition(beanName)) {
			return (!BeanFactoryUtils.isFactoryDereference(name) || isFactoryBean(name));
		}
		BeanFactory parentBeanFactory = getParentBeanFactory();
		return (parentBeanFactory != null && parentBeanFactory.containsBean(originalBeanName(name)));
	}

	@Override
	public boolean isPrototype(String name) throws NoSuchBeanDefinitionException {
		String beanName = transformedBeanName(name);
	
		BeanFactory parentBeanFactory = getParentBeanFactory();
		if (parentBeanFactory != null && !containsBeanDefinition(beanName)) {
			return parentBeanFactory.isPrototype(originalBeanName(name));
		}
		
		RootBeanDefinition mbd = getMergedLocalBeanDefinition(beanName);
		if (mbd.isPrototype()) {
			return (!BeanFactoryUtils.isFactoryDereference(name) || isFactoryBean(beanName, mbd));
		}
		else {
			if (BeanFactoryUtils.isFactoryDereference(name)) {
				return false;
			}
			if (isFactoryBean(beanName, mbd)) {
				final FactoryBean<?> factoryBean = (FactoryBean<?>) getBean(FACTORY_BEAN_PREFIX + beanName);
				if (System.getSecurityManager() != null) {
					return AccessController.doPrivileged(new PrivilegedAction<Boolean>() {
						@Override
						public Boolean run() {
							if (factoryBean instanceof SmartFactoryBean) {
								if (((SmartFactoryBean<?>) factoryBean).isPrototype()) {
									return true;
								}
							}
							if (!factoryBean.isSingleton()) {
								return true;
							}
							return false;
						}
					}, getAccessControlContext());
				} 
				else {
					if (factoryBean instanceof SmartFactoryBean) {
						if (((SmartFactoryBean<?>) factoryBean).isPrototype()) {
							return true;
						}
					}
					if (!factoryBean.isSingleton()) {
						return true;
					}
					return false;
				}
			}
			else {
				return false;
			}
		}
	}

	@Override
	public boolean isTypeMatch(String name, Class<?> targetType) throws NoSuchBeanDefinitionException {
		String beanName = transformedBeanName(name);
		Class<?> typeToMatch = (targetType != null ? targetType : Object.class);
		
		Object beanInstance = getSingleton(beanName, false);
		if (beanInstance != null) {
			if (beanInstance instanceof FactoryBean) {
				if (!BeanFactoryUtils.isFactoryDereference(name)) {
					Class<?> type = getTypeForFactoryBean((FactoryBean<?>) beanInstance);
					return (type != null && typeToMatch.isAssignableFrom(type));
				} 
				else {
					return typeToMatch.isAssignableFrom(beanInstance.getClass());
				}
			} 
			else {
				return !BeanFactoryUtils.isFactoryDereference(name) 
					&& typeToMatch.isAssignableFrom(beanInstance.getClass());
			}
		} 
		else if (containsSingleton(beanName) && !containsBeanDefinition(beanName)) {
			return false;
		}
		else {
			BeanFactory parentBeanFactory = getParentBeanFactory();
			if (parentBeanFactory != null && !containsBeanDefinition(beanName)) {
				return parentBeanFactory.isTypeMatch(originalBeanName(name), targetType);
			}
			
			RootBeanDefinition mbd = getMergedLocalBeanDefinition(beanName);
			
			BeanDefinitionHolder dbd = mbd.getDecoratedDefinition();
			if (dbd != null && !BeanFactoryUtils.isFactoryDereference(name)) {
				RootBeanDefinition tbd = getMergedBeanDefinition(dbd.getBeanName(), dbd.getBeanDefinition(), mbd);
				Class<?> targetClass = predictBeanType(dbd.getBeanName(), tbd, FactoryBean.class, typeToMatch);
				if (targetClass != null && !FactoryBean.class.isAssignableFrom(targetClass)) {
					return typeToMatch.isAssignableFrom(targetClass);
				}
			}
			
			Class<?> beanClass = predictBeanType(beanName, mbd, FactoryBean.class, typeToMatch);
			if (beanClass == null) {
				return false;
			}
			
			if (FactoryBean.class.isAssignableFrom(beanClass)) {
				if (!BeanFactoryUtils.isFactoryDereference(name)) {
					Class<?> type = getTypeForFactoryBean(beanName, mbd);
					return (type != null && typeToMatch.isAssignableFrom(type));
				} 
				else {
					return typeToMatch.isAssignableFrom(beanClass);
				}
			} 
			else {
				return !BeanFactoryUtils.isFactoryDereference(name) 
					&& typeToMatch.isAssignableFrom(beanClass);
			}
		}
	}
	
	/**
	 * Return an instance, which may be shared or independent, of the specified bean.
	 * @param name
	 * @param requiredType
	 * @param args
	 * @return
	 * @throws BeansException
	 */
	public <T> T getBean(String name, Class<T> requiredType, Object... args) throws BeansException {
		return doGetBean(name, null, args, false);
	}
	
	/**
	 * Return an instance, which may be shared or independent, of the specified bean.
	 * @param name
	 * @param requiredType
	 * @param args
	 * @param typeCheckOnly
	 * @return
	 * @throws BeansException
	 */
	protected <T> T doGetBean(final String name, final Class<T> requiredType, final Object[] args, boolean typeCheckOnly) throws BeansException {
		final String beanName = transformedBeanName(name);
		Object bean;
		
		Object sharedInstance = getSingleton(beanName);
		if (sharedInstance != null && args == null) {
			if (logger.isDebugEnabled()) {
				if (isSingletonCurrentlyInCreation(beanName)) {
					logger.debug(String.format(
						"Returning eagerly cached instance of singleton bean '%s' that is not fully initialized yet - "
						+ "a consequence of a circular referencee", beanName));
				} 
				else {
					logger.debug(String.format("Returning cached instance of singleton bean '%s'", beanName));
				}
			}
			bean = getObjectForBeanInstance(sharedInstance, name, beanName, null);
		} 
		else {
			
			if (isPrototypeCurrentlyInCreation(beanName)) {
				throw new BeanCurrentlyInCreationException(beanName);
			}
			
			BeanFactory parentBeanFactory = getParentBeanFactory();
			if (parentBeanFactory != null && !containsBeanDefinition(beanName)) {
				String nameToLookup = originalBeanName(name);
				if (args != null) {
					return (T) parentBeanFactory.getBean(nameToLookup, args);
				} 
				else {
					return parentBeanFactory.getBean(nameToLookup, requiredType);
				}
			}
			
			if (!typeCheckOnly) {
				markBeanAsCreated(beanName);
			}
			
			final RootBeanDefinition mbd = getMergedLocalBeanDefinition(beanName);
			checkMergedBeanDefinition(mbd, beanName, args);
			
			String[] dependsOn = mbd.getDependsOn();
			if (dependsOn != null) {
				for (String dependsOnBean : dependsOn) {
					getBean(dependsOnBean);
					registerDependentBean(dependsOnBean, beanName);
				}
			}
			
			if (mbd.isSingleton()) {
				sharedInstance = getSingleton(beanName, new ObjectFactory() {
					@Override
					public Object getObject() throws BeansException {
						try {
							return createBean(beanName, mbd, args);
						} 
						catch (BeansException ex) {
							destroySingleton(beanName);
							throw ex;
						}
					}
				});
				bean = getObjectForBeanInstance(sharedInstance, name, beanName, mbd);
			}
			else if (mbd.isPrototype()) {
				Object prototypeInstance = null;
				try {
					beforePrototypeCreation(beanName);
					prototypeInstance = createBean(beanName, mbd, args);
				} 
				finally {
					afterPrototypeCreation(beanName);
				}
				bean = getObjectForBeanInstance(prototypeInstance, name, beanName, mbd);
			}
			else {
				String scopeName = mbd.getScope();
				final Scope scope = scopes.get(scopeName);
				if (scope == null) {
					throw new IllegalStateException(String.format("No Scope registered for scope '%s'", scopeName));
				}
				try {
					Object scopedInstance = scope.get(beanName, new ObjectFactory() {
						@Override
						public Object getObject() throws BeansException {
							beforePrototypeCreation(beanName);
							try {
								return createBean(beanName, mbd, args);
							}
							finally {
								afterPrototypeCreation(beanName);
							}
						}
					});
					bean = getObjectForBeanInstance(scopedInstance, name, beanName, mbd);
				} 
				catch (IllegalStateException ex) {
					throw new BeanCreationException(beanName, String.format(
						"Scope '%s' is not active for the current thread; consider defining a scoped proxy "
						+ "for this bean if you intend to refer to it from a singleton", scopeName), ex);
				}
			}
		}
		
		if (requiredType != null && bean != null && !requiredType.isAssignableFrom(bean.getClass())) {
			try {
				return getTypeConverter().convertIfNecessary(bean, requiredType);
			} 
			catch (TypeMismatchException ex) {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format(
						"Failed to convert bean '%s' to required type [%s]", 
							name, ClassUtils.getQualifiedName(requiredType)), ex);
				}
				throw new BeanNotOfRequiredTypeException(name, requiredType, bean.getClass());
			}
		}
		return (T) bean;
	}
	
	
	//-----------------------------------------------------------
	// Implementation of HierarchicalBeanFactory interface
	//-----------------------------------------------------------
	
	@Override
	public boolean containsLocalBean(String name) {
		String beanName = transformedBeanName(name);
		return ((containsSingleton(beanName) || containsBeanDefinition(beanName)) 
			&& (!BeanFactoryUtils.isFactoryDereference(name) || isFactoryBean(beanName)));
	}

	@Override
	public BeanFactory getParentBeanFactory() {
		return parentBeanFactory;
	}
	
	
	//-----------------------------------------------------------
	// Implementation of ConfigurableBeanFactory interface
	//-----------------------------------------------------------
	
	@Override
	public void addBeanPostProcessor(BeanPostProcessor beanPostProcessor) {
		Assert.notNull(beanPostProcessor, "BeanPostProcessor must not be null");
		
		beanPostProcessors.remove(beanPostProcessor);
		beanPostProcessors.add(beanPostProcessor);
		if (beanPostProcessor instanceof InstantiationAwareBeanPostProcessor) {
			hasInstantiationAwareBeanPostProcessors = true;
		}
		if (beanPostProcessor instanceof DestructionAwareBeanPostProcessor) {
			hasDestructionAwareBeanPostProcessors = true;
		}
	}

	@Override
	public int getBeanPostProcessorCount() {
		return beanPostProcessors.size();
	}
	
	/**
	 * Return the list of BeanPostProcessors that will get applied
	 * to beans created with this factory.
	 * @return
	 */
	public List<BeanPostProcessor> getBeanPostProcessors() {
		return beanPostProcessors;
	}
	
	/**
	 * Return whether this factory holds a InstantiationAwareBeanPostProcessor
	 * that will get applied to singleton beans on shutdown.
	 * @return
	 */
	public boolean hasInstantiationAwareBeanPostProcessors() {
		return hasInstantiationAwareBeanPostProcessors;
	}

	/**
	 * Return whether this factory holds a DestructionAwareBeanPostProcessor
	 * that will get applied to singleton beans on shutdown.
	 * @return
	 */
	public boolean hasDestructionAwareBeanPostProcessors() {
		return hasDestructionAwareBeanPostProcessors;
	}

	@Override
	public void registerCustomEditor(Class<?> requiredType, PropertyEditor propertyEditor) {
		
	}

	@Override
	public void setParentBeanFactory(BeanFactory parentBeanFactory) throws IllegalStateException {
		if (this.parentBeanFactory != null && this.parentBeanFactory != parentBeanFactory) {
			throw new IllegalStateException("Already associated with parent BeanFactory: " + this.parentBeanFactory);
		}
		this.parentBeanFactory = parentBeanFactory;
	}

	@Override
	public void setBeanClassLoader(ClassLoader beanClassLoader) {
		this.beanClassLoader = (beanClassLoader != null ? beanClassLoader : ClassUtils.getDefaultClassLoader());
	}

	@Override
	public ClassLoader getBeanClassLoader() {
		return beanClassLoader;
	}

	@Override
	public void setTempClassLoader(ClassLoader tempClassLoader) {
		this.tempClassLoader = tempClassLoader;
	}

	@Override
	public ClassLoader getTempClassLoader() {
		return tempClassLoader;
	}

	@Override
	public void setCacheBeanMetadata(boolean cacheBeanMetadata) {
		this.cacheBeanMetadata = cacheBeanMetadata;
	}

	@Override
	public boolean isCacheBeanMetadata() {
		return cacheBeanMetadata;
	}

	@Override
	public void setBeanExpressionResolver(BeanExpressionResolver resolver) {
		this.beanExpressionResolver = resolver;
	}

	@Override
	public BeanExpressionResolver getBeanExpressionResolver() {
		return beanExpressionResolver;
	}

	@Override
	public void setConversionService(ConversionService conversionService) {
		this.conversionService = conversionService;
	}

	@Override
	public ConversionService getConversionService() {
		return conversionService;
	}

	@Override
	public void addPropertyEditorRegistrar(PropertyEditorRegistrar registrar) {
		Assert.notNull(registrar, "PropertyEditorRegistrar must not be null");
		propertyEditorRegistrars.add(registrar);
	}

	@Override
	public void copyRegisteredEditorsTo(PropertyEditorRegistry registry) {
		registerCustomEditors(registry);
	}

	@Override
	public void setTypeConverter(TypeConverter typeConverter) {
		this.typeConverter = typeConverter;
	}

	@Override
	public TypeConverter getTypeConverter() {
		TypeConverter customConverter = getCustomTypeConverter();
		if (customConverter != null) {
			return customConverter;
		} 
		else {
			SimpleTypeConverter typeConverter = new SimpleTypeConverter();
			typeConverter.setConversionService(getConversionService());
			registerCustomEditors(typeConverter);
			return typeConverter;
		}
	}

	@Override
	public void addEmbeddedValueResolver(StringValueResolver valueResolver) {
		Assert.notNull(valueResolver, "StringValueResolver must not be null");
		
		embeddedValueResolvers.add(valueResolver);
	}

	@Override
	public String resolveEmbeddedValue(String value) {
		String result = value;
		for (StringValueResolver resolver : embeddedValueResolvers) {
			result = resolver.resolveStringValue(result);
		}
		return result;
	}

	@Override
	public void registerScope(String scopeName, Scope scope) {
		Assert.notNull(scopeName, "Scope name must not be null");
		Assert.notNull(scope, "Scope must not be null");
		
		if (SCOPE_SINGLETON.equals(scopeName) || SCOPE_PROTOTYPE.equals(scopeName)) {
			throw new IllegalArgumentException("Cannot replace existing scopes 'singleton' and 'prototype'");
		}
		scopes.put(scopeName, scope);
	}

	@Override
	public String[] getRegisteredScopeNames() {
		return StringUtils.toStringArray(scopes.keySet());
	}
	
	/**
	 * 
	 * @param scopeName
	 * @return
	 */
	public Scope getRegisteredScope(String scopeName) {
		Assert.notNull(scopeName, "Scope name must not be null");
		return scopes.get(scopeName);
	}

	@Override
	public AccessControlContext getAccessControlContext() {
		return securityContextProvider != null 
			? securityContextProvider.getAccessControlContext() 
			: AccessController.getContext();
	}

	/**
	 * Set the security context provider for this bean factory.
	 * @param securityContextProvider
	 */
	public void setSecurityContextProvider(SecurityContextProvider securityContextProvider) {
		this.securityContextProvider = securityContextProvider;
	}

	@Override
	public void copyConfigurationFrom(ConfigurableBeanFactory otherFactory) {
		Assert.notNull(otherFactory, "BeanFactory must not be null");
		
		setBeanClassLoader(otherFactory.getBeanClassLoader());
		setCacheBeanMetadata(otherFactory.isCacheBeanMetadata());
		setBeanExpressionResolver(otherFactory.getBeanExpressionResolver());
		if (otherFactory instanceof AbstractBeanFactory) {
			AbstractBeanFactory otherAbstractFactory = (AbstractBeanFactory) otherFactory;
			customEditors.putAll(otherAbstractFactory.customEditors);
			propertyEditorRegistrars.addAll(otherAbstractFactory.propertyEditorRegistrars);
			beanPostProcessors.addAll(otherAbstractFactory.beanPostProcessors);
			hasInstantiationAwareBeanPostProcessors = hasInstantiationAwareBeanPostProcessors 
				|| otherAbstractFactory.hasInstantiationAwareBeanPostProcessors;
			hasDestructionAwareBeanPostProcessors = hasDestructionAwareBeanPostProcessors
				|| otherAbstractFactory.hasDestructionAwareBeanPostProcessors;
			scopes.putAll(otherAbstractFactory.scopes);
			securityContextProvider = otherAbstractFactory.securityContextProvider;
		} 
		else {
			setTypeConverter(otherFactory.getTypeConverter());
		}
		
	}

	@Override
	public BeanDefinition getMergedBeanDefinition(String name) throws BeansException {
		String beanName = transformedBeanName(name);
		
		if (!containsBeanDefinition(beanName) && getParentBeanFactory() instanceof ConfigurableBeanFactory) {
			return ((ConfigurableBeanFactory) getParentBeanFactory()).getMergedBeanDefinition(beanName);
		}
		
		return getMergedLocalBeanDefinition(beanName);
	}

	@Override
	public boolean isFactoryBean(String name) throws NoSuchBeanDefinitionException {
		String beanName = transformedBeanName(name);
		
		Object beanInstance = getSingleton(beanName, false);
		if (beanInstance != null) {
			return (beanInstance instanceof FactoryBean);
		}
		else if (containsSingleton(beanName)) {
			return false;
		}
		
		if (!containsBeanDefinition(beanName) && getParentBeanFactory() instanceof ConfigurableBeanFactory) {
			return ((ConfigurableBeanFactory) getParentBeanFactory()).isFactoryBean(name);
		}
		
		return isFactoryBean(beanName, getMergedLocalBeanDefinition(beanName));
	}

	@Override
	public boolean isCurrentlyInCreation(String beanName) {
		Assert.notNull(beanName, "Bean name must not be null");
		
		return isSingletonCurrentlyInCreation(beanName) 
			|| isPrototypeCurrentlyInCreation(beanName);
	}

	@Override
	public void destroyScopedBean(String beanName) {
		RootBeanDefinition mbd = getMergedLocalBeanDefinition(beanName);
		if (mbd.isSingleton() || mbd.isPrototype()) {
			throw new IllegalArgumentException(String.format(
				"Bean name '%s' does not correspond to an object in a mutable scope", beanName));
		}
		
		String scopeName = mbd.getScope();
		Scope scope = scopes.get(scopeName);
		if (scope == null) {
			throw new IllegalStateException(String.format("No scope SPI registered for scope '%s'", scopeName));
		}
		Object bean = scope.remove(beanName);
		if (bean != null) {
			destroyBean(beanName, bean, mbd);
		}
	}

	@Override
	public void destroyBean(String beanName, Object beanInstance) {
		destroyBean(beanName, beanInstance, getMergedLocalBeanDefinition(beanName));
	}
	
	//-------------------------------------------------------------------
	// Implementation methods
	//-------------------------------------------------------------------

	/**
	 * Return the bean name, stripping out the factory dereference prefix if necessary,
	 * and resolving aliases to canonical names.
	 * @param name
	 * @return
	 */
	protected String transformedBeanName(String name) {
		return canonicalName(BeanFactoryUtils.transformedBeanName(name));
	}
	
	/**
	 * Determine the original bean name, resolving locally defined aliases to canonical names.
	 * @param name
	 * @return
	 */
	protected String originalBeanName(String name) {
		String beanName = transformedBeanName(name);
		if (name.startsWith(FACTORY_BEAN_PREFIX)) {
			beanName = FACTORY_BEAN_PREFIX + beanName;
		}
		return beanName;
	}
	
	/**
	 * Initialize the given BeanWrapper with the custom editors registered
	 * with this factory. To be called for BeanWrapper that will create
	 * and populate bean instances.
	 * @param bw
	 */
	protected void initBeanWrapper(BeanWrapper bw) {
		bw.setConversionService(getConversionService());
		registerCustomEditors(bw);
	}
	
	/**
	 * Initialize the given PropertyEditorRegistry with the custom editors
	 * that have been registered with this BeanFactory.
	 * @param registry
	 */
	protected void registerCustomEditors(PropertyEditorRegistry registry) {
		PropertyEditorRegistrySupport registrySupport = registry instanceof PropertyEditorRegistrySupport 
			? (PropertyEditorRegistrySupport) registry : null;
		if (registrySupport != null) {
			registrySupport.useConfigValueEditors();
		}
		if (!this.propertyEditorRegistrars.isEmpty()) {
			for (PropertyEditorRegistrar registrar : propertyEditorRegistrars) {
				try {
					registrar.registerCustomEditors(registry);
				} 
				catch (BeanCreationException ex) {
					Throwable rootCause = ex.getMostSpecificCause();
					if (rootCause instanceof BeanCurrentlyInCreationException) {
						BeanCreationException bce = (BeanCreationException) rootCause;
						if (isCurrentlyInCreation(bce.getBeanName())) {
							if (logger.isDebugEnabled()) {
								logger.debug(String.format(
									"PropertyEditorRegistrar [%s] failed because it tried to obtain currently created bean '%s': %s", 
										registrar.getClass().getName(), ex.getBeanName(), ex.getMessage()));
							}
							onSuppressedException(ex);
							continue;
						}
					}
					throw ex;
				}
			}
		}
		if (!this.customEditors.isEmpty()) {
			for (Entry<Class<?>, Class<? extends PropertyEditor>> entry : customEditors.entrySet()) {
				Class<?> requiredType = entry.getKey();
				Class<? extends PropertyEditor> editorClass = entry.getValue();
				registry.registerCustomEditor(requiredType, BeanUtils.instantiateClass(editorClass));
			}
		}
	}
	
	/**
	 * Return a merged RootBeanDefinition, traversing the parent bean definition
	 * if the specified bean corresponds to a child bean definition.
	 * @param beanName
	 * @return
	 * @throws BeansException
	 */
	protected RootBeanDefinition getMergedLocalBeanDefinition(String beanName) throws BeansException {
		RootBeanDefinition mbd = mergedBeanDefinitions.get(beanName);
		if (mbd != null) {
			return mbd;
		}
		return getMergedBeanDefinition(beanName, getBeanDefinition(beanName));
	}
	
	/**
	 * Return a RootBeanDefinition for the given top-level bean, by merging with
	 * the parent if the given bean's definition is a child bean definition.
	 * @param beanName
	 * @param bd
	 * @return
	 * @throws BeanDefinitionStoreException
	 */
	protected RootBeanDefinition getMergedBeanDefinition(String beanName, BeanDefinition bd) throws BeanDefinitionStoreException {
		return getMergedBeanDefinition(beanName, bd, null);
	}
	
	/**
	 * Return a RootBeanDefinition for the given bean, by merging with the 
	 * parent if the given bean's definition is a child bean definition.
	 * @param beanName
	 * @param bd
	 * @param containingBd
	 * @return
	 * @throws BeanDefinitionStoreException
	 */
	protected RootBeanDefinition getMergedBeanDefinition(String beanName, BeanDefinition bd, BeanDefinition containingBd) throws BeanDefinitionStoreException {
		synchronized (mergedBeanDefinitions) {
			RootBeanDefinition mbd = null;
			
			if (containingBd == null) {
				mbd = mergedBeanDefinitions.get(beanName);
			}
			
			if (mbd == null) {
				if (bd.getParentName() == null) {
					if (bd instanceof RootBeanDefinition) {
						mbd = ((RootBeanDefinition) bd).cloneBeanDefinition();
					} 
					else {
						mbd = new RootBeanDefinition(bd);
					}
				} 
				else {
					BeanDefinition pbd;
					try {
						String parentBeanName = transformedBeanName(bd.getParentName());
						if (!beanName.equals(parentBeanName)) {
							pbd = getMergedBeanDefinition(parentBeanName);
						} 
						else {
							if (getParentBeanFactory() instanceof ConfigurableBeanFactory) {
								pbd = ((ConfigurableBeanFactory) getParentBeanFactory()).getMergedBeanDefinition(parentBeanName);
							} 
							else {
								throw new NoSuchBeanDefinitionException(bd.getParentName(), String.format(
									"Parent name '%s' is equals to bean name '%s': cannot be resolved without an AbstractBeanFactory parent", 
										bd.getParentName(), beanName));
							}
						}
					} 
					catch (NoSuchBeanDefinitionException ex) {
						throw new BeanDefinitionStoreException(bd.getResourceDescription(), beanName, 
							String.format("Could not resolve parent bean definition '%s'", bd.getParentName()), ex);
					}
					mbd = new RootBeanDefinition(pbd);
					mbd.overrideFrom(bd);
				}
				
				if (!StringUtils.hasLength(mbd.getScope())) {
					mbd.setScope(RootBeanDefinition.SCOPE_SINGLETON);
				}
				
				if (containingBd != null && !containingBd.isSingleton() && mbd.isSingleton()) {
					mbd.setScope(containingBd.getScope());
				}
				
				if (containingBd == null && isCacheBeanMetadata() && isBeanEligibleForMetadataCaching(beanName)) {
					mergedBeanDefinitions.put(beanName, mbd);
				}
			}
			return mbd;
		}
	}
	
	/**
	 * Check the given merged bean definition,
	 * potentially throwing validation exceptions.
	 * @param mbd
	 * @param beanName
	 * @param args
	 * @throws BeanDefinitionStoreException
	 */
	protected void checkMergedBeanDefinition(RootBeanDefinition mbd, String beanName, Object[] args) throws BeanDefinitionStoreException {
		if (mbd.isAbstract()) {
			throw new BeanIsAbstractException(beanName);
		}
		
		if (args != null && !mbd.isPrototype()) {
			throw new BeanDefinitionStoreException(
				"Can only specify arguments for the getBean method when referring to a prototype bean definition");
		}
	}
	
	/**
	 * Resolve the bean class for the specified bean definition,
	 * resolving a bean class name into a Class reference and storing
	 * the resolved Class in the bean definition for further use.
	 * @param mbd
	 * @param beanName
	 * @param typesToMatch
	 * @return
	 */
	protected Class<?> resolveBeanClass(final RootBeanDefinition mbd, String beanName, final Class<?>... typesToMatch) throws CannotLoadBeanClassException {
		try {
			if (mbd.hasBeanClass()) {
				return mbd.getBeanClass();
			}
			if (System.getSecurityManager() != null) {
				return AccessController.doPrivileged(new PrivilegedExceptionAction<Class<?>>() {
					@Override
					public Class<?> run() throws Exception {
						return doResolveBeanClass(mbd, typesToMatch);
					}
				}, getAccessControlContext());
			}
			else {
				return doResolveBeanClass(mbd, typesToMatch);
			}
		} 
		catch (PrivilegedActionException ex) {
			throw new CannotLoadBeanClassException(mbd.getResourceDescription(), beanName, mbd.getBeanClassName(), (ClassNotFoundException)ex.getException());
		}
		catch (ClassNotFoundException ex) {
			throw new CannotLoadBeanClassException(mbd.getResourceDescription(), beanName, mbd.getBeanClassName(), ex);
		}
		catch (LinkageError ex) {
			throw new CannotLoadBeanClassException(mbd.getResourceDescription(), beanName, mbd.getBeanClassName(), ex);
		}
	}
	
	private Class<?> doResolveBeanClass(RootBeanDefinition mbd, Class<?>... typesToMatch) throws ClassNotFoundException {
		if (!ObjectUtils.isEmpty(typesToMatch)) {
			ClassLoader tempClassLoader = getTempClassLoader();
			if (tempClassLoader != null) {
				if (tempClassLoader instanceof DecoratingClassLoader) {
					DecoratingClassLoader dcl = (DecoratingClassLoader) tempClassLoader;
					for (Class<?> typeToMatch : typesToMatch) {
						dcl.excludeClass(typeToMatch.getName());
					}
				}
				String className = mbd.getBeanClassName();
				return (className != null ? ClassUtils.forName(className, tempClassLoader) : null);
			}
		}
		return mbd.resolveBeanClass(getBeanClassLoader());
	}

	/**
	 * Evaluate the given String as contained in a bean definition,
	 * potentially resolving it as an expression.
	 * @param value
	 * @param beanDefintion
	 * @return
	 */
	protected Object evaluateBeanDefinitionString(String value, BeanDefinition beanDefintion) {
		if (beanExpressionResolver == null) {
			return value;
		}
		Scope scope = (beanDefintion != null ? getRegisteredScope(beanDefintion.getScope()) : null);
		return beanExpressionResolver.evaluate(value, new BeanExpressionContext(this, scope))
	}
	
	/**
	 * Predict the eventual bean type (of the processed bean instance) for the 
	 * specified bean.
	 * @param beanName
	 * @param mbd
	 * @param typesToMatch
	 * @return
	 */
	protected Class<?> predictBeanType(String beanName, RootBeanDefinition mbd, Class<?>... typesToMatch) {
		if (mbd.getFactoryMethodName() != null) {
			return null;
		}
		return resolveBeanClass(mbd, beanName, typesToMatch);
	}
	
	/**
	 * Check whether the given bean is defined as a FactoryBean.
	 * @param beanName
	 * @param mbd
	 * @return
	 */
	protected boolean isFactoryBean(String beanName, RootBeanDefinition mbd) {
		Class<?> beanClass = predictBeanType(beanName, mbd, FactoryBean.class);
		return (beanClass != null && FactoryBean.class.isAssignableFrom(beanClass));
	}
	
	/**
	 * Determine the bean type for the given FactoryBean definition, as far as possible.
	 * @return
	 */
	protected Class<?> getTypeForFactoryBean(String beanName, RootBeanDefinition mbd) {
		if (!mbd.isSingleton()) {
			return null;
		}
		try {
			FactoryBean<?> factoryBean = doGetBean(FACTORY_BEAN_PREFIX + beanName, FactoryBean.class, null, true);
			return getTypeForFactoryBean(factoryBean);
		} 
		catch (BeanCreationException ex) {
			if (logger.isDebugEnabled()) {
				logger.debug("Ignoring bean creation exception on FactoryBean type check: " + ex);
			}
			onSuppressedException(ex);
			return null;
		}
	}
	
	/**
	 * Mark the specified bean as already created.
	 * @param beanName
	 */
	protected void markBeanAsCreated(String beanName) {
		alreadyCreated.add(beanName);
	}
	
	/**
	 * Get the object for the given bean instancee, either the bean
	 * instance itself or its created object in case of a FactoryBean.
	 * @param beanInstance
	 * @param name
	 * @param beanName
	 * @param mbd
	 * @return
	 */
	protected Object getObjectForBeanInstance(Object beanInstance, String name, String beanName, RootBeanDefinition mbd) {
		
		if (BeanFactoryUtils.isFactoryDereference(name) && !(beanInstance instanceof FactoryBean)) {
			throw new BeanIsNotAFactoryException(transformedBeanName(name), beanInstance.getClass());
		}
		
		if (BeanFactoryUtils.isFactoryDereference(name) || !(beanInstance instanceof FactoryBean)) {
			return beanInstance;
		}
		
		Object object = null;
		if (mbd == null) {
			object = getCachedObjectForFactoryBean(beanName);
		}
		if (object == null) {
			FactoryBean<?> factory = (FactoryBean<?>) beanInstance;
			if (mbd == null && containsBeanDefinition(beanName)) {
				mbd = getMergedLocalBeanDefinition(beanName);
			}
			boolean synthetic = (mbd != null & mbd.isSynthetic());
			object = getObjectFromFactoryBean(factory, beanName, !synthetic);
		}
		return object;
	}
	
	/**
	 * Callback before prototype creation.
	 * @param beanName
	 */
	@SuppressWarnings("unchecked")
	protected void beforePrototypeCreation(String beanName) {
		Object obj = prototypesCurrentlyInCreation.get();
		if (obj == null) {
			prototypesCurrentlyInCreation.set(beanName);
		}
		else if (obj instanceof String) {
			Set<String> beanNameSet = new HashSet<String>(2);
			beanNameSet.add((String) obj);
			beanNameSet.add(beanName);
			prototypesCurrentlyInCreation.set(beanNameSet);
		}
		else {
			Set<String> beanNameSet = (Set<String>) obj;
			beanNameSet.add(beanName);
		}
	}
	
	/**
	 * Callback after prototype creation.
	 * @param beanName
	 */
	@SuppressWarnings("unchecked")
	protected void afterPrototypeCreation(String beanName) {
		Object obj = prototypesCurrentlyInCreation.get();
		if (obj instanceof String) {
			prototypesCurrentlyInCreation.remove();
		} 
		else if (obj instanceof Set) {
			Set<String> beanNameSet = (Set<String>) obj;
			beanNameSet.remove(beanName);
			if (beanNameSet.isEmpty()) {
				prototypesCurrentlyInCreation.remove();
			}
		}
	}
	
	/**
	 * Return whether the specified prototype bean is currently in creation.
	 * @param beanName
	 * @return
	 */
	protected final boolean isPrototypeCurrentlyInCreation(String beanName) {
		Object object = prototypesCurrentlyInCreation.get();
		if (object != null) {
			if (object.equals(beanName)) {
				return true;
			}
			if (object instanceof Set && ((Set<?>) object).contains(beanName)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Destroy the given bean instance occording to the given bean definition.
	 * @param beanName
	 * @param beanInstance
	 * @param mbd
	 */
	protected void destroyBean(String beanName, Object beanInstance, RootBeanDefinition mbd) {
		new DisposableBeanAdapter(beanInstance, beanName, mbd, getBeanPostProcessors(), getAccessControlContext()).destroy();
	}
	
	/**
	 * Return the custom TypeConverter to use, if any.
	 * @return
	 */
	protected TypeConverter getCustomTypeConverter() {
		return typeConverter;
	}
	
	//-----------------------------------------------------------
	// Abstract methods to be implemented by subclasses
	//-----------------------------------------------------------

	/**
	 * 
	 * @param beanName
	 * @return
	 */
	protected abstract boolean containsBeanDefinition(String beanName);

	/**
	 * 
	 * @param beanName
	 * @return
	 */
	protected abstract BeanDefinition getBeanDefinition(String beanName) throws BeansException;
	
	/**
	 * 
	 * @param beanName
	 * @param mergedBeanDefinition
	 * @param args
	 * @return
	 * @throws BeanCreationException
	 */
	protected abstract Object createBean(String beanName, RootBeanDefinition mergedBeanDefinition, Object[] args) throws BeanCreationException;

}
