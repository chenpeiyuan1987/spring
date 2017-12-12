package org.yuan.study.spring.beans.factory.support;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.yuan.study.spring.beans.factory.BeanCreationException;
import org.yuan.study.spring.beans.factory.BeanCreationNotAllowedException;
import org.yuan.study.spring.beans.factory.BeanCurrentlyInCreationException;
import org.yuan.study.spring.beans.factory.DisposableBean;
import org.yuan.study.spring.beans.factory.ObjectFactory;
import org.yuan.study.spring.beans.factory.config.SingletonBeanRegistry;
import org.yuan.study.spring.core.SimpleAliasRegistry;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.StringUtils;

public class DefaultSingletonBeanRegistry extends SimpleAliasRegistry implements SingletonBeanRegistry {

	/** Internal marker for a null singleton object */
	protected static final Object NULL_OBJECT = new Object();
	
	/** Logger available to subclasses */
	protected final Log logger = LogFactory.getLog(getClass());
	
	/** Cache of singleton objects: bean name --> bean instance */
	private final Map<String, Object> singletonObjects = new ConcurrentHashMap<String, Object>();
	
	/** Cache of singleton factories: bean name --> ObjectFactory */
	private final Map<String, ObjectFactory<?>> singletonFactories = new HashMap<String, ObjectFactory<?>>();
	
	/** Cache of early singleton objects: bean name --> bean instance */
	private final Map<String, Object> earlysSingletonObjects = new HashMap<String, Object>();
	
	/** Set of registered singletons, containing the bean names in registration order */
	private final Set<String> registeredSingletons = new LinkedHashSet<String>(16);
	
	/** Names of beans that are currently in creation */
	private final Set<String> singletonsCurrentlyInCreation = Collections.synchronizedSet(new HashSet<String>());
	
	/** List of suppressed Exceptions, available for associating related causes */
	private Set<Exception> suppressedExceptions;
	
	/** Flag that indicates whether we're currently within destroySingletons */
	private boolean singletonsCurrentlyInDestruction = false;
	
	/** Disposable bean instances: bean name --> disposable instance */
	private final Map<String, Object> disposableBeans = new LinkedHashMap<String, Object>();
	
	/** Map between containing bean names: bean name --> Set of bean names that the bean contains */
	private final Map<String, Set<String>> containedBeanMap = new ConcurrentHashMap<String, Set<String>>();
	
	/** Map between dependent bean names: bean name --> Set of dependent bean names */
	private final Map<String, Set<String>> dependentBeanMap = new ConcurrentHashMap<String, Set<String>>();
	
	/** Map between depending bean names: bean name --> Set of bean names for the bean's dependencies */
	private final Map<String, Set<String>> dependenciesForBeanMap = new ConcurrentHashMap<String, Set<String>>();
	
	//--------------------------------------------------------------------------------------------------
	// Implementation of SingletonBeanRegistry Methods
	//--------------------------------------------------------------------------------------------------
	
	@Override
	public void registerSingleton(String beanName, Object singletonObject) throws IllegalStateException {
		Assert.notNull(beanName, "BeanName must not be null");
		
		synchronized (singletonObjects) {
			Object oldObject = singletonObjects.get(beanName);
			if (oldObject != null) {
				throw new IllegalStateException(String.format(
					"Could not register object [%s] under bean name '%s': there is already object [%s] bound", 
						singletonObject, beanName, oldObject));
			}
			addSingleton(beanName, singletonObject);
		}
	}

	@Override
	public Object getSingleton(String beanName) {
		return getSingleton(beanName, true);
	}

	@Override
	public boolean containsSingleton(String beanName) {
		return (singletonObjects.containsKey(beanName));
	}

	@Override
	public String[] getSingletonNames() {
		synchronized (singletonObjects) {
			return StringUtils.toStringArray(registeredSingletons);
		}
	}

	@Override
	public int getSingletonCount() {
		synchronized (singletonObjects) {
			return registeredSingletons.size();
		}
	}
	
	//-----------------------------------------------------------------------------
	// Implementation of Methods
	//-----------------------------------------------------------------------------
	
	/**
	 * Return the singleton object registered under the given name, 
	 * creating and registering a new one if none registered yet.
	 * @param beanName
	 * @param singletonFactory
	 * @return
	 */
	public Object getSingleton(String beanName, ObjectFactory<?> singletonFactory) {
		Assert.notNull(beanName, "BeanName must not be null");
		
		synchronized (singletonObjects) {
			Object singletonObject = singletonObjects.get(beanName);
			if (singletonObject == null) {
				if (singletonsCurrentlyInDestruction) {
					throw new BeanCreationNotAllowedException(beanName, 
						"Singleton bean creation not allowed while the singletons of this factory are in destruction "
						+ "(Do not request a bean from a BeanFactory in a destroy method implementation!)");
				}
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Creating shared instance of singleton bean '%s'", beanName));
				}
				beforeSingletonCreation(beanName);
				boolean recordSuppressedExceptions = (suppressedExceptions == null);
				if (recordSuppressedExceptions) {
					suppressedExceptions = new LinkedHashSet<Exception>();
				}
				try {
					singletonObject = singletonFactory.getObject();
				} 
				catch (BeanCreationException ex) {
					if (recordSuppressedExceptions) {
						for (Exception suppressedException : suppressedExceptions) {
							ex.addRelatedCause(suppressedException);
						}
					}
				}
				finally {
					if (recordSuppressedExceptions) {
						suppressedExceptions = null;
					}
					afterSingletonCreation(beanName);
				}
				addSingleton(beanName, singletonObject);
			}
			return (singletonObject != NULL_OBJECT ? singletonObject : null);
		}
	}
	
	/**
	 * Return whether the specified singleton bean is currently in creation.
	 * @param beanName
	 * @return
	 */
	public final boolean isSingletonCurrentlyInCreation(String beanName) {
		return singletonsCurrentlyInCreation.contains(beanName);
	}
	
	/**
	 * Add the given bean to the list of disposable beans in this registry.
	 * @param beanName
	 * @param bean
	 */
	public void registerDisposableBean(String beanName, DisposableBean bean) {
		synchronized (disposableBeans) {
			disposableBeans.put(beanName, bean);
		}
	}
	
	/**
	 * Register a containment relationship between two beans,
	 * e.g. between an inner bean and its containing outer bean.
	 * @param containedBeanName
	 * @param containingBeanName
	 */
	public void registerContainedBean(String containedBeanName, String containingBeanName) {
		synchronized (containedBeanMap) {
			Set<String> containedBeans = containedBeanMap.get(containingBeanName);
			if (containedBeans == null) {
				containedBeans = new LinkedHashSet<String>(8);
				containedBeanMap.put(containingBeanName, containedBeans);
				containedBeans.add(containedBeanName);
			}
		}
		registerDependentBean(containedBeanName, containingBeanName);
	}
	
	/**
	 * Register a dependent bean for the given bean,
	 * to be destroyed before the given bean is destroyed.
	 * @param beanName
	 * @param dependentBeanName
	 */
	public void registerDependentBean(String beanName, String dependentBeanName) {
		String canonicalName = canonicalName(beanName);
		synchronized (dependentBeanMap) {
			Set<String> dependentBeans = dependentBeanMap.get(canonicalName);
			if (dependentBeans == null) {
				dependentBeans = new LinkedHashSet<String>(8);
				dependentBeanMap.put(canonicalName, dependentBeans);
			}
			dependentBeans.add(dependentBeanName);
		}
		synchronized (dependenciesForBeanMap) {
			Set<String> dependenciesForBean = dependenciesForBeanMap.get(dependentBeanName);
			if (dependenciesForBean == null) {
				dependenciesForBean = new LinkedHashSet<String>(8);
				dependenciesForBeanMap.put(dependentBeanName, dependenciesForBean);
			}
			dependenciesForBean.add(canonicalName);
		}
	}
	
	/**
	 * Return the names of all beans which depend on the specified bean, if any.
	 * @param beanName
	 * @return
	 */
	public String[] getDependentBeans(String beanName) {
		Set<String> dependentBeans = dependentBeanMap.get(beanName);
		if (dependentBeans == null) {
			return new String[0];
		}
		return StringUtils.toStringArray(dependentBeans);
	}
	
	/**
	 * Return the names of all beans that the specified bean depends on, if any.
	 * @param beanName
	 * @return
	 */
	public String[] getDependenciesForBean(String beanName) {
		Set<String> dependenciesForBean = dependenciesForBeanMap.get(beanName);
		if (dependenciesForBean == null) {
			return new String[0];
		}
		return dependenciesForBean.toArray(new String[dependenciesForBean.size()]);
	}
	
	public void destroySingletons() {
		if (logger.isInfoEnabled()) {
			logger.info("Destroying  singletons in " + this);
		}
		synchronized (singletonObjects) {
			singletonsCurrentlyInDestruction = true;
		}
		synchronized (disposableBeans) {
			String[] disposableBeanNames = StringUtils.toStringArray(disposableBeans.keySet());
			for (String disposableBeanName : disposableBeanNames) {
				destroySingleton(disposableBeanName);
			}
		}
		
		containedBeanMap.clear();
		dependentBeanMap.clear();
		dependenciesForBeanMap.clear();
		
		synchronized (singletonObjects) {
			singletonObjects.clear();
			singletonFactories.clear();
			earlysSingletonObjects.clear();
			registeredSingletons.clear();
			singletonsCurrentlyInDestruction = false;
		}
	}
	
	/**
	 * Destroy the given bean. Delegates to 'destroyBean'
	 * if a corresponding disposable bean instance is found.
	 * @param beanName
	 */
	public void destroySingleton(String beanName) {
		removeSingleton(beanName);
		
		DisposableBean disposableBean;
		synchronized (disposableBeans) {
			disposableBean = (DisposableBean) disposableBeans.remove(beanName);
		}
		destroyBean(beanName, disposableBean);
	}
	
	/**
	 * Add the given singleton object to the singleton cache of this factory.
	 * @param beanName
	 * @param singletonObject
	 */
	protected void addSingleton(String beanName, Object singletonObject) {
		synchronized (singletonObjects) {
			singletonObjects.put(beanName, (singletonObject != null ? singletonObject : NULL_OBJECT));
			singletonFactories.remove(beanName);
			earlysSingletonObjects.remove(beanName);
			registeredSingletons.add(beanName);
		}
	}
	
	/**
	 * Add the given singleton factory for building the specified singleton if necessary.
	 * @param beanName
	 * @param singletonFactory
	 */
	protected void addSingletonFactory(String beanName, ObjectFactory<?> singletonFactory) {
		Assert.notNull(singletonFactory, "Singleton factory must not be null");
		
		synchronized (singletonObjects) {
			if (singletonObjects.containsKey(beanName)) {
				singletonFactories.put(beanName, singletonFactory);
				earlysSingletonObjects.remove(beanName);
				registeredSingletons.add(beanName);
			}
		}
	}
	
	/**
	 * Return the singleton object registered under the given name.
	 * @param beanName
	 * @param allowEarlyReference
	 * @return
	 */
	protected Object getSingleton(String beanName, boolean allowEarlyReference) {
		Object singletonObject = singletonObjects.get(beanName);
		if (singletonObject == null) {
			synchronized (singletonObjects) {
				singletonObject = earlysSingletonObjects.get(beanName);
				if (singletonObject == null && allowEarlyReference) {
					ObjectFactory<?> singletonFactory = singletonFactories.get(beanName);
					if (singletonFactory != null) {
						singletonObject = singletonFactory.getObject();
						earlysSingletonObjects.put(beanName, singletonObject);
						singletonFactories.remove(beanName);
					}
				}
			}
		}
		return (singletonObject != NULL_OBJECT ? singletonObject : null);
	}
	
	/**
	 * Register an Exception that happened to get suppressed during the creation of a
	 * singleton bean instance, e.g. a temporary circular referencee resolution problem.
	 * @param ex
	 */
	protected void onSuppressedException(Exception ex) {
		synchronized (singletonObjects) {
			if (suppressedExceptions != null) {
				suppressedExceptions.add(ex);
			}
		}
	}
	
	/**
	 * Remove the bean with the given name from the singleton cache of this factory,
	 * to be able to clean up eager registration of a singleton if creation failed.
	 * @param beanName
	 */
	protected void removeSingleton(String beanName) {
		synchronized (singletonObjects) {
			singletonObjects.remove(beanName);
			singletonFactories.remove(beanName);
			earlysSingletonObjects.remove(beanName);
			registeredSingletons.remove(beanName);
		}
	}
	
	/**
	 * Callback before singleton creation.
	 * @param beanName
	 */
	protected void beforeSingletonCreation(String beanName) {
		if (!singletonsCurrentlyInCreation.add(beanName)) {
			throw new BeanCurrentlyInCreationException(beanName);
		}
	}
	
	/**
	 * Callback after singleton creation.
	 * @param beanName
	 */
	protected void afterSingletonCreation(String beanName) {
		if (!singletonsCurrentlyInCreation.remove(beanName)) {
			throw new IllegalStateException(String.format(
				"Singleton '%s' isn't currently in creation", beanName));
		}
	}
	
	/**
	 * Determine whether a dependent bean has been registered for the given name.
	 * @param beanName
	 * @return
	 */
	protected boolean hasDependentBean(String beanName) {
		return dependentBeanMap.containsKey(beanName);
	}
	
	/**
	 * Destroy the given bean. Must destroy beans that depend on the given
	 * bean before the bean itself. Should not throw any exceptions.
	 * @param beanName
	 * @param bean
	 */
	protected void destroyBean(String beanName, DisposableBean bean) {
		Set<String> dependencies = dependentBeanMap.remove(beanName);
		if (dependencies != null) {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format(
					"Retrieved dependent beans for bean '%s': %s", beanName, dependencies));
			}
			for (String dependentBeanName : dependencies) {
				destroySingleton(dependentBeanName);
			}
		}
		
		if (bean != null) {
			try {
				bean.destroy();
			} 
			catch (Throwable ex) {
				logger.error(String.format(
					"Destroy method on bean with name '%s' threw an exception", beanName), ex);
			}
		}
		
		Set<String> containedBeans = containedBeanMap.remove(beanName);
		if (containedBeans !=null) {
			for (String containedBeanName : containedBeans) {
				destroySingleton(containedBeanName);
			}
		}
		
		synchronized (dependentBeanMap) {
			Iterator<Entry<String, Set<String>>> iter = dependentBeanMap.entrySet().iterator();
			while (iter.hasNext()) {
				Entry<String, Set<String>> entry = iter.next();
				Set<String> dependenciesToClean = entry.getValue();
				dependenciesToClean.remove(beanName);
				if (dependenciesToClean.isEmpty()) {
					iter.remove();
				}
			}
		}
		
		dependenciesForBeanMap.remove(beanName);
	}
	
	/**
	 * Expose the singleton mutex to subclasses.
	 * @return
	 */
	protected final Object getSingletonMutex() {
		return singletonObjects;
	}
}
