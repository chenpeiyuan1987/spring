package org.yuan.study.spring.beans.factory.support;

import java.beans.PropertyEditor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.yuan.study.spring.beans.BeanUtils;
import org.yuan.study.spring.beans.BeanWrapper;
import org.yuan.study.spring.beans.BeanWrapperImpl;
import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.TypeMismatchException;
import org.yuan.study.spring.beans.factory.BeanCreationException;
import org.yuan.study.spring.beans.factory.BeanCurrentlyInCreationException;
import org.yuan.study.spring.beans.factory.BeanDefinitionStoreException;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.BeanFactoryUtils;
import org.yuan.study.spring.beans.factory.BeanIsAbstractException;
import org.yuan.study.spring.beans.factory.BeanIsNotAFactoryException;
import org.yuan.study.spring.beans.factory.BeanNotOfRequiredTypeException;
import org.yuan.study.spring.beans.factory.DisposableBean;
import org.yuan.study.spring.beans.factory.FactoryBean;
import org.yuan.study.spring.beans.factory.FactoryBeanNotInitializedException;
import org.yuan.study.spring.beans.factory.NoSuchBeanDefinitionException;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.beans.factory.config.BeanPostProcessor;
import org.yuan.study.spring.beans.factory.config.ConfigurableBeanFactory;
import org.yuan.study.spring.beans.factory.config.DestructionAwareBeanPostProcessor;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.StringUtils;

public abstract class AbstractBeanFactory extends FactoryBeanRegistrySupport implements ConfigurableBeanFactory {
	
	/** Logger available to subclasses */
	protected final Log logger = LogFactory.getLog(getClass());
	
	/** Parent bean factory, for bean inheritance support */
	private  BeanFactory parentBeanFactory;
	
	/** Cache of singletons: bean name --> bean instance */
	private final Map<String,Object> singletonCache = new HashMap<String,Object>();
	
	/** Names of beans that are currently in creation */
	private final Set<String> currentlyInCreation = Collections.synchronizedSet(new HashSet<String>());
	
	/** BeanPostProcessors to apply in createBean */
	private final List<BeanPostProcessor> beanPostProcessors = new ArrayList<BeanPostProcessor>();
	
	/** Indicates whether any DestructionAwareBeanPostProcessors have been registered */
	private boolean hasDestructionAwareBeanPostProcessors;
	
	/** Disposable bean instances: bean name --> disposable instance */
	private final Map<String,Object> disposableBeans = new HashMap<String,Object>();
	
	/** Map from alias to canonical bean name */
	private final Map<String,String> aliasMap = new HashMap<String,String>();
	
	/** Map between dependent bean names: bean name --> dependent bean name */
	private final Map<String,Set<String>> dependentBeanMap = new HashMap<String,Set<String>>();
	
	/**  */
	private final Map<Class<?>,PropertyEditor> customEditors = new HashMap<Class<?>,PropertyEditor>();
	
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
	// Implementation methods
	//-------------------------------------------------------------------
	
	/**
	 * 
	 * @param name
	 * @param args
	 * @return
	 * @throws BeansException
	 */
	public Object getBean(String name, Object[] args) throws BeansException {
		return getBean(name, null, args);
	}
	
	/**
	 * 
	 * @param name
	 * @param requiredType
	 * @param args
	 * @return
	 * @throws BeansException
	 */
	public Object getBean(String name, Class<?> requiredType, Object[] args) throws BeansException {
		String beanName = transformedBeanName(name);
		Object bean = null;
		
		synchronized (this.singletonCache) {
			bean = this.singletonCache.get(beanName);
		}
		if (bean != null) {
			if (isSingletonCurrentlyInCreation(beanName)) {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Returning eagerly cached instance of singleton bean '%s' that is not fully initialized yet - a consequence of a circular reference", beanName));
				}
			}
			else {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Returning cached instance of singleton bean '%s'", beanName));
				}
			}
			bean = getObjectForSharedInstance(name, bean);
		}
		
		else {
			if (isSingletonCurrentlyInCreation(beanName)) {
				throw new BeanCurrentlyInCreationException(beanName);
			}
			
			if (getParentBeanFactory() != null && !containsBeanDefinition(beanName)) {
				if (getParentBeanFactory() instanceof AbstractBeanFactory) {
					return ((AbstractBeanFactory) getParentBeanFactory()).getBean(name, requiredType, args);
				}
				else if (args == null) {
					return getParentBeanFactory().getBean(name, requiredType);
				}
				else {
					throw new NoSuchBeanDefinitionException(beanName, 
						"Cannot delegate to parent BeanFactory because it does not supported passed-in arguments");
				}
			}
			
			RootBeanDefinition mergedBeanDefinition = getMergedBeanDefinition(beanName, false);
			checkMergedBeanDefinition(mergedBeanDefinition, beanName, requiredType, args);
		
			if (mergedBeanDefinition.isSingleton()) {
				synchronized (this.singletonCache) {
					bean = this.singletonCache.get(beanName);
					if (bean == null) {
						if (logger.isDebugEnabled()) {
							logger.debug(String.format("Creating shared instance of singleton bean '%s'", beanName));
						}
						this.currentlyInCreation.add(beanName);
						try {
							bean = createBean(beanName, mergedBeanDefinition, args);
							addSingleton(beanName, bean);
						}
						catch (BeansException ex) {
							destroyDisposableBean(beanName);
							throw ex;
						}
						finally {
							this.currentlyInCreation.remove(beanName);
						}
					}
				}
				bean = getObjectForSharedInstance(name, bean);
			}
			else {
				bean = createBean(beanName, mergedBeanDefinition, args);
			}
		}
		

		if (requiredType != null && !requiredType.isAssignableFrom(bean.getClass())) {
			throw new BeanNotOfRequiredTypeException(name, requiredType, bean.getClass());
		}
		return bean;
	}
	
	/**
	 * 
	 * @param beanName
	 * @param dependentBeanName
	 */
	protected void registerDependentBean(String beanName, String dependentBeanName) {
		synchronized (this.dependentBeanMap) {
			Set<String> dependencies = this.dependentBeanMap.get(beanName);
			if (dependencies == null) {
				dependencies = new HashSet<String>();
				this.dependentBeanMap.put(beanName, dependencies);
			}
			dependencies.add(dependentBeanName);
		}
	}
	
	/**
	 * 
	 * @param beanName
	 * @param bean
	 */
	protected void registerDisposableBean(String beanName, DisposableBean bean) {
		synchronized (this.disposableBeans) {
			this.disposableBeans.put(beanName, bean);
		}
	}
	
	/**
	 * 
	 * @param beanName
	 * @param bean
	 * @param mergedBeanDefinition
	 */
	protected void registerDisposableBeanIfNecessary(final String beanName, final Object bean, final RootBeanDefinition mergedBeanDefinition) {
		if (mergedBeanDefinition.isSingleton()) {
			final boolean isDisposableBean = (bean instanceof DisposableBean);
			final boolean hasDestroyMethod = (mergedBeanDefinition.getDestroyMethodName() != null);
			
			if (isDisposableBean || hasDestroyMethod || hasDestructionAwareBeanPostProcessors()) {
				int counter = 1;
				String id = beanName;
				synchronized (this.disposableBeans) {
					while (this.disposableBeans.containsKey(id)) {
						counter++;
						id = beanName + "#" + counter;
					}
				}
				
				registerDisposableBean(id, new DisposableBean() {

					@Override
					public void destroy() throws Exception {
						if (logger.isDebugEnabled()) {
							logger.debug(String.format("Applying DestructionAwareBeanPostProcessor to bean with name '%s'", beanName));
						}
						if (hasDestructionAwareBeanPostProcessors()) {
							for (BeanPostProcessor beanPostProcessor : getBeanPostProcessors()) {
								if (beanPostProcessor instanceof DestructionAwareBeanPostProcessor) {
									((DestructionAwareBeanPostProcessor) beanPostProcessor).postProcessBeforeDestruction(bean, beanName);
								}
							}
						}
						
						if (isDisposableBean) {
							if (logger.isDebugEnabled()) {
								logger.debug(String.format("Invoking destroy() on bean with name '%s'", beanName));
							}
							((DisposableBean) bean).destroy();
						}
						
						if (hasDestroyMethod) {
							if (logger.isDebugEnabled()) {
								logger.debug(String.format("Invoking custom destroy method on bean with name '%s'", beanName));
							}
							invokeCustomDestroyMethod(beanName, bean, mergedBeanDefinition.getDestroyMethodName(), 
								mergedBeanDefinition.isEnforceDestroyMethod());
						}
					}
					
				});
			}
			
			String[] dependsOn = mergedBeanDefinition.getDependsOn();
			if (dependsOn != null) {
				for (String dependOn : dependsOn) {
					registerDependentBean(dependOn, beanName);
				}
			}
		}
	}
	
	/**
	 * 
	 * @param name
	 * @return
	 */
	protected boolean isFactoryDereference(String name) {
		return BeanFactoryUtils.isFactoryDereference(name);
	}
	
	/**
	 * 
	 * @param name
	 * @return
	 */
	protected String transformedBeanName(String name) {
		String beanName = BeanFactoryUtils.transformedBeanName(name);
		
		synchronized (this.aliasMap) {
			String canonicalName = beanName;
			String resolvedName = null;
			do {
				resolvedName = (String) this.aliasMap.get(canonicalName);
				if (resolvedName != null) {
					canonicalName = resolvedName;
				}
			} while (resolvedName != null);
			return canonicalName;
		}
	}
	
	/**
	 * 
	 * @param name
	 * @param bean
	 * @return
	 */
	protected Object getObjectForSharedInstance(String name, Object bean) {
		String beanName = transformedBeanName(name);
		
		if (isFactoryDereference(name) && !(bean instanceof FactoryBean)) {
			throw new BeanIsNotAFactoryException(beanName, bean.getClass());
		}
		
		if (bean instanceof FactoryBean) {
			if (!isFactoryDereference(name)) {
				FactoryBean factory = (FactoryBean) bean;
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Bean with name '%s' is a factory bean", beanName));
				}
				try {
					bean = factory.getObject();
				}
				catch (Exception ex) {
					throw new BeanCreationException(beanName, "FactoryBean threw exception on object creation", ex);
				}
				if (bean == null) {
					throw new FactoryBeanNotInitializedException(beanName, "FactoryBean returned null object: " + 
						"probably not fully initialized (maybe due to circular bean reference)");
				}
			}
			else {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Calling code asked for FactoryBean instance for name '%s'", beanName));
				}
			}
		}
		
		return bean;
	}
	
	/**
	 * 
	 * @param name
	 * @param includingAncestors
	 * @return
	 */
	protected RootBeanDefinition getMergedBeanDefinition(String name, boolean includingAncestors) {
		String beanName = transformedBeanName(name);
		
		if (includingAncestors && !containsBeanDefinition(beanName) 
			&& getParentBeanFactory() instanceof AbstractBeanFactory) {
			return ((AbstractBeanFactory) getParentBeanFactory()).getMergedBeanDefinition(beanName, true);
		}
		
		return getMergedBeanDefinition(beanName, getBeanDefinition(beanName));
	}
	
	/**
	 * 
	 * @param beanName
	 * @param bd
	 * @return
	 */
	protected RootBeanDefinition getMergedBeanDefinition(String beanName, BeanDefinition bd) {
		if (bd instanceof RootBeanDefinition) {
			return (RootBeanDefinition) bd;
		}
		else if (bd instanceof ChildBeanDefinition) {
			ChildBeanDefinition cbd = (ChildBeanDefinition) bd;
			RootBeanDefinition pbd = null;
			try {
				if (!beanName.equals(cbd.getParentName())) {
					pbd = getMergedBeanDefinition(cbd.getParentName(), true);
				}
				else {
					if (getParentBeanFactory() instanceof AbstractBeanFactory) {
						AbstractBeanFactory parentFactory = (AbstractBeanFactory) getParentBeanFactory();
						pbd = parentFactory.getMergedBeanDefinition(cbd.getParentName(), true);
					}
					else {
						throw new NoSuchBeanDefinitionException(cbd.getParentName(), 
							String.format("Parent name '%s' is equals to bean name '%s': cannot be resolved without an AbstractBeanFactory parent", cbd.getParentName(), beanName));
					}
				}
			}
			catch (NoSuchBeanDefinitionException ex) {
				throw new BeanDefinitionStoreException(cbd.getResourceDescription(), beanName, 
					String.format("Could not resolve parent bean definition '%s'", cbd.getParentName()), ex);
			}
			
			RootBeanDefinition rbd = new RootBeanDefinition(pbd);
			rbd.overrideFrom(cbd);
			
			try {
				rbd.validate();
			}
			catch (BeanDefinitionValidationException ex) {
				throw new BeanDefinitionStoreException(rbd.getResourceDescription(), beanName, 
					"Validation of bean definition failed", ex);
			}
			
			return rbd;
		}
		else {
			throw new BeanDefinitionStoreException(bd.getResourceDescription(), beanName, 
				"Definition is neither a RootBeanDefinition nor a ChildBeanDefinition");
		}
	}
	
	/**
	 * 
	 * @param mergedBeanDefinition
	 * @param beanName
	 * @param requiredType
	 * @param args
	 */
	protected void checkMergedBeanDefinition(RootBeanDefinition mergedBeanDefinition, String beanName, Class<?> requiredType, Object[] args) {
		if (mergedBeanDefinition.isAbstract()) {
			throw new BeanIsAbstractException(beanName);
		}
		
		if (mergedBeanDefinition.hasBeanClass()) {
			Class<?> beanClass = mergedBeanDefinition.getBeanClass();
			if (requiredType != null && mergedBeanDefinition.getFactoryMethodName() == null 
				&& !FactoryBean.class.isAssignableFrom(beanClass) && !requiredType.isAssignableFrom(beanClass)) {
				throw new BeanNotOfRequiredTypeException(beanName, requiredType, beanClass);
			}
		}
		
		if (args != null) {
			if (mergedBeanDefinition.isSingleton()) {
				throw new BeanDefinitionStoreException("Cannot specify arguments in the getBean() method when referring to a singleton bean definition");
			}
			else if (mergedBeanDefinition.getFactoryMethodName() == null) {
				throw new BeanDefinitionStoreException("Can only specify arguments in the getBean() method in conjunction with a factory method");
			}
		}
	}
	
	/**
	 * 
	 * @param beanName
	 * @param singletonObject
	 */
	protected void addSingleton(String beanName, Object singletonObject) {
		Assert.hasText(beanName, "Bean name must not be empty");
		Assert.notNull(singletonObject, "Singleton object must not be null");
		synchronized (this.singletonCache) {
			this.singletonCache.put(beanName, singletonObject);
		}
	}
	
	/**
	 * 
	 * @param beanName
	 */
	protected void removeSingleton(String beanName) {
		Assert.hasText(beanName, "Bean name must not be empty");
		synchronized (this.singletonCache) {
			this.singletonCache.remove(beanName);
		}
	}
	
	/**
	 * 
	 * @param beanName
	 */
	private void destroyDisposableBean(String beanName) {
		removeSingleton(beanName);
		
		Object disposableBean = null;
		synchronized (this.disposableBeans) {
			disposableBean = this.disposableBeans.remove(beanName);
		}
		destroyBean(beanName, disposableBean);
	}
	
	/**
	 * 
	 * @param beanName
	 * @return
	 */
	protected boolean isSingletonCurrentlyInCreation(String beanName) {
		return this.currentlyInCreation.contains(beanName);
	}
	
	/**
	 * 
	 * @param beanName
	 * @param bean
	 */
	protected void destroyBean(String beanName, Object bean) {
		Set<String> dependencies = null;
		synchronized (this.dependentBeanMap) {
			dependencies = this.dependentBeanMap.remove(beanName);
		}
		
		if (dependencies != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("Retrieved dependent beans for bean '%s': ", beanName, dependencies));
			}
			for (String dependentBeanName : dependencies) {
				destroyDisposableBean(dependentBeanName);
			}
		}
		
		if (bean instanceof DisposableBean) {
			try {
				((DisposableBean) bean).destroy();
			}
			catch (Throwable ex) {
				logger.error(String.format("Destroy method on bean with name '%s' threw an exception", beanName), ex);
			}
		}
	}
	
	/**
	 * 
	 * @return
	 */
	protected boolean hasDestructionAwareBeanPostProcessors() {
		return this.hasDestructionAwareBeanPostProcessors;
	}
	
	/**
	 * 
	 * @param beanWrapper
	 */
	protected void initBeanWrapper(BeanWrapper beanWrapper) {
		for (Entry<Class<?>,PropertyEditor> entry : getCustomEditors().entrySet()) {
			beanWrapper.registerCustomEditor(entry.getKey(), entry.getValue());
		}
	}
	
	/**
	 * 
	 * @param value
	 * @param targetType
	 * @return
	 * @throws TypeMismatchException
	 */
	protected Object doTypeConversionIfNecessary(Object value, Class<?> targetType) throws TypeMismatchException {
		BeanWrapperImpl bw = new BeanWrapperImpl();
		initBeanWrapper(bw);
		return doTypeConversionIfNecessary(value, targetType, bw);
	}
	
	/**
	 * 
	 * @param value
	 * @param targetType
	 * @param bw
	 * @return
	 * @throws TypeMismatchException
	 */
	protected Object doTypeConversionIfNecessary(Object value, Class<?> targetType, BeanWrapperImpl bw) throws TypeMismatchException {
		if (!getCustomEditors().isEmpty()) {
			synchronized (getCustomEditors()) {
				return bw.doTypeConversionIfNecessary(value, targetType);
			}
		}
		else {
			return bw.doTypeConversionIfNecessary(value, targetType);
		}
	}
	
	/**
	 * 
	 * @param beanName
	 * @param bean
	 * @param destroyMethodName
	 * @param enforceDestroyMethod
	 */
	protected void invokeCustomDestroyMethod(String beanName, Object bean, String destroyMethodName, boolean enforceDestroyMethod) {
		Method destroyMethod = BeanUtils.findDeclaredMethodWithMinimalParameters(bean.getClass(), destroyMethodName);
		if (destroyMethod == null) {
			if (enforceDestroyMethod) {
				logger.error(String.format("Couldn't find a destroy method named '%s' on bean with name '%s'", destroyMethodName, beanName));
			}
		}
		else {
			Class<?>[] paramTypes = destroyMethod.getParameterTypes();
			if (paramTypes.length > 1) {
				logger.error(String.format(
					"Method '%s' of bean '%s' has more than one parameter - not supported as destroy method", 
					destroyMethodName, beanName));
			}
			else if (paramTypes.length == 1 && !paramTypes[0].equals(boolean.class)) {
				logger.error(String.format(
					"Method '%s' of bean '%s' has a non-boolean parameter - not supported as destroy method", 
					destroyMethodName, beanName));
			}
			else {
				Object[] args = new Object[paramTypes.length];
				if (paramTypes.length == 1) {
					args[0] = Boolean.TRUE;
				}
				if (!Modifier.isPublic(destroyMethod.getModifiers())) {
					destroyMethod.setAccessible(true);
				}
				try {
					destroyMethod.invoke(bean, args);
				}
				catch (InvocationTargetException ex) {
					logger.error(String.format("Couldn't invoke destroy method '%s' of bean with name '%s'", destroyMethodName, beanName), ex.getTargetException());
				}
				catch (Throwable ex) {
					logger.error(String.format("Couldn't invoke destroy method '%s' of bean with name '%s'", destroyMethodName, beanName), ex);
				}
				
			}
		}
	}
	
	/**
	 * 
	 * @return
	 */
	public int getSingletonCount() {
		synchronized (this.singletonCache) {
			return this.singletonCache.size();
		}
	}
	
	/**
	 * 
	 * @return
	 */
	public String[] getSingletonNames() {
		synchronized (this.singletonCache) {
			return StringUtils.toStringArray(this.singletonCache.keySet());
		}
	}
	
	/**
	 * 
	 * @param name
	 * @return
	 * @throws BeansException
	 */
	public RootBeanDefinition getMergedBeanDefinition(String name) throws BeansException {
		return getMergedBeanDefinition(name, false);
	}
	
	/**
	 * 
	 * @return
	 */
	public Map<Class<?>,PropertyEditor> getCustomEditors() {
		return this.customEditors;
	}

	/**
	 * 
	 * @param name
	 * @return
	 * @throws NoSuchBeanDefinitionException
	 */
	public boolean isFactoryBean(String name) throws NoSuchBeanDefinitionException {
		String beanName = transformedBeanName(name);
		
		synchronized (this.singletonCache) {
			Object beanInstance = this.singletonCache.get(beanName);
			if (beanInstance != null) {
				return (beanInstance instanceof FactoryBean);
			}
		}
		
		if (!containsBeanDefinition(beanName) && getParentBeanFactory() instanceof AbstractBeanFactory) {
			return ((AbstractBeanFactory) getParentBeanFactory()).isFactoryBean(name);
		}
		
		RootBeanDefinition bd = getMergedBeanDefinition(beanName, false);
		return (bd.hasBeanClass() && FactoryBean.class.equals(bd.getBeanClass()));
	}
	
	/**
	 * 
	 * @return
	 */
	public List<BeanPostProcessor> getBeanPostProcessors() {
		return this.beanPostProcessors;
	}
	
	
	//-------------------------------------------------------------------
	// Implementation of BeanFactory interface
	//-------------------------------------------------------------------
	
	@Override
	public Object getBean(String name) throws BeansException {
		return getBean(name, null, null);
	}

	@Override
	public Object getBean(String name, Class<?> requiredType) throws BeansException {
		return getBean(name, requiredType, null);
	}
	
	@Override
	public Class<?> getType(String name) {
		try {
			String beanName = transformedBeanName(name);
			Class<?> beanClass = null;
			
			Object beanInstance = null;
			synchronized (this.singletonCache) {
				beanInstance = this.singletonCache.get(beanName);
			}
			if (beanInstance != null) {
				beanClass = beanInstance.getClass();
			}
			else {
				if (getParentBeanFactory() != null && !containsBeanDefinition(beanName)) {
					return getParentBeanFactory().getType(name);
				}
				
				RootBeanDefinition mergedBeanDefinition = getMergedBeanDefinition(beanName, false);
				if (mergedBeanDefinition.getFactoryMethodName() != null) {
					return getTypeForFactoryMethod(name, mergedBeanDefinition);
				}
				if (!mergedBeanDefinition.hasBeanClass()) {
					return null;
				}
				beanClass = mergedBeanDefinition.getBeanClass();
			}
			
			if (FactoryBean.class.isAssignableFrom(beanClass) && !isFactoryDereference(name)) {
				FactoryBean factoryBean = (FactoryBean) getBean(FACTORY_BEAN_PREFIX + beanName);
				return factoryBean.getObjectType();
			}
			
			return beanClass;
		}
		catch (BeanCreationException ex) {
			if (ex.contains(BeanCurrentlyInCreationException.class) 
				|| ex.contains(FactoryBeanNotInitializedException.class)) {
				logger.debug("Ignoring BeanCreationException on FactoryBean type check", ex);
			}
			throw ex;
		}
	}

	@Override
	public boolean isSingleton(String name) {
		String beanName = transformedBeanName(name);
		Class<?> beanClass = null;
		boolean singleton = true;
		
		Object beanInstance = null;
		synchronized (this.singletonCache) {
			beanInstance = this.singletonCache.get(beanName);
		}
		
		if (beanInstance != null) {
			beanClass = beanInstance.getClass();
			singleton = true;
		}
		else {
			if (getParentBeanFactory() != null && !containsBeanDefinition(beanName)) {
				return getParentBeanFactory().isSingleton(name);
			}
			
			RootBeanDefinition bd = getMergedBeanDefinition(beanName, false);
			if (bd.hasBeanClass()) {
				beanClass = bd.getBeanClass();
			}
			singleton = bd.isSingleton();
		}
		
		if (beanClass != null && FactoryBean.class.isAssignableFrom(beanClass) 
			&& !isFactoryDereference(name)) {
			FactoryBean factoryBean = (FactoryBean) getBean(FACTORY_BEAN_PREFIX + beanName);
			return factoryBean.isSingleton();
		}
		
		return singleton;
	}

	@Override
	public boolean containsBean(String name) {
		if (containsLocalBean(name)) {
			return true;
		}
		
		BeanFactory parentBeanFactory = getParentBeanFactory();
		if(parentBeanFactory != null) {
			return parentBeanFactory.containsBean(name);
		}
		return false;
	}

	@Override
	public String[] getAliases(String name) {
		String beanName = transformedBeanName(name);
		
		if (containsSingleton(beanName) || containsBeanDefinition(beanName)) {
			List<String> aliases = new ArrayList<String>();
			synchronized (this.aliasMap) {
				for (Entry<String,String> entry : this.aliasMap.entrySet()) {
					if (entry.getValue().equals(beanName)) {
						aliases.add(entry.getKey());
					}
				}
			}
			return StringUtils.toStringArray(aliases);
		}
		else {
			BeanFactory parentBeanFactory = getParentBeanFactory();
			if (parentBeanFactory != null) {
				return parentBeanFactory.getAliases(name);
			}
			throw new NoSuchBeanDefinitionException(beanName, toString());
		}
	}

	
	//-----------------------------------------------------------
	// Implementation of HierarchicalBeanFactory interface
	//-----------------------------------------------------------
	
	@Override
	public boolean containsLocalBean(String name) {
		String beanName = transformedBeanName(name);
		return (containsSingleton(beanName) || containsBeanDefinition(beanName));
	}

	@Override
	public BeanFactory getParentBeanFactory() {
		return this.parentBeanFactory;
	}

	
	//-----------------------------------------------------------
	// Implementation of ConfigurableBeanFactory interface
	//-----------------------------------------------------------
	
	@Override
	public void addBeanPostProcessor(BeanPostProcessor beanPostProcessor) {
		Assert.notNull(beanPostProcessor, "BeanPostProcessor must not be null");
		this.beanPostProcessors.add(beanPostProcessor);
		if (beanPostProcessor instanceof DestructionAwareBeanPostProcessor) {
			this.hasDestructionAwareBeanPostProcessors = true;
		}
	}

	@Override
	public boolean containsSingleton(String beanName) {
		Assert.hasText(beanName, "Bean name must not be empty");
		synchronized (this.singletonCache) {
			return this.singletonCache.containsKey(beanName);
		}
	}

	@Override
	public void destroySingletons() {
		if (logger.isInfoEnabled()) {
			logger.info(String.format("Destroying singletons in factory {%s}", this));
		}
		synchronized (this.singletonCache) {
			synchronized (this.disposableBeans) {
				String[] disposableBeanNames = StringUtils.toStringArray(this.disposableBeans.keySet());
				for (String disposableBeanName : disposableBeanNames) {
					destroyDisposableBean(disposableBeanName);
				}
			}
			this.singletonCache.clear();
		}
	}

	@Override
	public int getBeanPostProcessorCount() {
		return this.beanPostProcessors.size();
	}

	@Override
	public void registerAlias(String beanName, String alias) {
		Assert.hasText(beanName, "Bean name must not be empty");
		Assert.hasText(alias, "Alias must not be empty");
		
		if (!alias.equals(beanName)) {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("Registering alias '%s' for bean with name '%s'", alias, beanName));
			}
			synchronized (this.aliasMap) {
				Object registeredName = this.aliasMap.get(alias);
				if (registeredName != null && !registeredName.equals(beanName)) {
					throw new BeanDefinitionStoreException(String.format(
						"Cannot register alias '%s' for bean name '%s': It's already registered for bean name '%s'.", 
						alias, beanName, registeredName));
				}
				this.aliasMap.put(alias, beanName);
			}
		}
		
	}

	@Override
	public void registerCustomEditor(Class<?> requiredType, PropertyEditor propertyEditor) {
		Assert.notNull(requiredType, "Required type must not be null");
		Assert.notNull(propertyEditor, "PropertyEditor must not be null");
		this.customEditors.put(requiredType,propertyEditor);
	}

	@Override
	public void registerSingleton(String beanName, Object singletonObject) {
		Assert.hasText(beanName, "Bean name must not be empty");
		Assert.notNull(singletonObject, "Singleton object must not be null");
		synchronized (this.singletonCache) {
			Object beanInstance = this.singletonCache.get(beanName);
			if (beanInstance != null) {
				throw new BeanDefinitionStoreException(String.format(
					"Could not register object [%s] under bean name '%s': there's already object [%s] ", 
					singletonObject, beanName, beanInstance));
			}
			addSingleton(beanName, singletonObject);
		}
	}

	@Override
	public void setParentBeanFactory(BeanFactory parentBeanFactory) {
		if (this.parentBeanFactory != null && this.parentBeanFactory != parentBeanFactory) {
			throw new IllegalStateException(String.format("Already associated with parent BeanFactory: %s", this.parentBeanFactory));
		}
		this.parentBeanFactory = parentBeanFactory;
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

	/**
	 * 
	 * @param beanName
	 * @param mergedBeanDefinition
	 * @return
	 */
	protected Class<?> getTypeForFactoryMethod(String beanName, RootBeanDefinition mergedBeanDefinition) {
		return null;
	}
}
