package org.yuan.study.spring.beans.factory.support;

import java.beans.PropertyEditor;
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
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.PropertyEditorRegistrar;
import org.yuan.study.spring.beans.PropertyEditorRegistry;
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
import org.yuan.study.spring.beans.factory.config.BeanExpressionResolver;
import org.yuan.study.spring.beans.factory.config.BeanPostProcessor;
import org.yuan.study.spring.beans.factory.config.ConfigurableBeanFactory;
import org.yuan.study.spring.beans.factory.config.Scope;
import org.yuan.study.spring.core.DecoratingClassLoader;
import org.yuan.study.spring.core.NamedThreadLocal;
import org.yuan.study.spring.core.convert.ConversionService;
import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.ObjectUtils;
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
		// TODO Auto-generated method stub
		return null;
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
				}
			}
			else {
				return false;
			}
		}
	}


	@Override
	public boolean isTypeMatch(String name, Class<?> targetType) throws NoSuchBeanDefinitionException {
		// TODO Auto-generated method stub
		return false;
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
		// TODO Auto-generated method stub
		
	}

	@Override
	public int getBeanPostProcessorCount() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public void registerCustomEditor(Class<?> requiredType,
			PropertyEditor propertyEditor) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setParentBeanFactory(BeanFactory parentBeanFactory)
			throws IllegalStateException {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setBeanClassLoader(ClassLoader beanClassLoader) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public ClassLoader getBeanClassLoader() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setTempClassLoader(ClassLoader tempClassLoader) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public ClassLoader getTempClassLoader() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setCacheBeanMetadata(boolean cacheBeanMetadata) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean isCacheBeanMetadata() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void setBeanExpressionResolver(BeanExpressionResolver resolver) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public BeanExpressionResolver getBeanExpressionResolver() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void setConversionService(ConversionService conversionService) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public ConversionService getConversionService() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void addPropertyEditorRegistrar(PropertyEditorRegistrar registrar) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void copyRegisteredEditorsTo(PropertyEditorRegistry registry) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setTypeConverter(TypeConverter typeConverter) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public TypeConverter getTypeConverter() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void addEmbeddedValueResolver(StringValueResolver valueResolver) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String resolveEmbeddedValue(String value) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void registerScope(String scopeName, Scope scope) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public String[] getRegisteredScopeNames() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void copyConfigurationFrom(ConfigurableBeanFactory otherFactory) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public BeanDefinition getMergedBeanDefinition(String beanName)
			throws NoSuchBeanDefinitionException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isFactoryBean(String name)
			throws NoSuchBeanDefinitionException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isCurrentlyInCreation(String beanName) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void destroyScopedBean(String beanName) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void destroyBean(String beanName, Object beanInstance) {
		// TODO Auto-generated method stub
		
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
		//TODO
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
			if (object instanceof Set && ((Set) object).contains(beanName)) {
				return true;
			}
		}
		return false;
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
