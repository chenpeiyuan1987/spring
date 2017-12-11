package org.yuan.study.spring.beans.factory.support;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.yuan.study.spring.beans.factory.BeanCurrentlyInCreationException;
import org.yuan.study.spring.beans.factory.DisposableBean;
import org.yuan.study.spring.beans.factory.ObjectFactory;
import org.yuan.study.spring.beans.factory.config.SingletonBeanRegistry;
import org.yuan.study.spring.core.SimpleAliasRegistry;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.StringUtils;

public class DefaultSingletonBeanRegistry extends SimpleAliasRegistry implements SingletonBeanRegistry {

	protected static final Object NULL_OBJECT = new Object();
	
	protected final Log Logger = LogFactory.getLog(getClass());
	
	private final Map<String, Object> singletonObjects = new ConcurrentHashMap<String, Object>();
	
	private final Map<String, ObjectFactory> singletonFactories = new HashMap<String, ObjectFactory>();
	
	private final Map<String, Object> earlysSingletonObjects = new HashMap<String, Object>();
	
	private final Set<String> registeredSingletons = new LinkedHashSet<String>(16);
	
	private final Set<String> singletonsCurrentlyInCreation = Collections.synchronizedSet(new HashSet<String>());
	
	private Set<Exception> suppressedExceptions;
	
	private boolean singletonsCurrentlyInDestruction = false;
	
	private final Map<String, Object> disposableBeans = new LinkedHashMap<String, Object>();
	
	private final Map<String, Set<String>> containedBeanMap = new ConcurrentHashMap<String, Set<String>>();
	
	private final Map<String, Set<String>> dependentBeanMap = new ConcurrentHashMap<String, Set<String>>();
	
	private final Map<String, Set<String>> dependenciesForBeanMap = new ConcurrentHashMap<String, Set<String>>();
	
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
	
	public Object getSingleton(String beanName, ObjectFactory singletonFactory) {
		Assert.notNull(beanName, "BeanName must not be null");
		
		synchronized (singletonObjects) {
			Object singletonObject = singletonObjects.get(beanName);
			if (singletonObject == null) {
				
			}
		}
	}
	
	public final boolean isSingletonCurrentlyInCreation(String beanName) {
		
	}
	
	public void registerDisposableBean(String beanName, DisposableBean bean) {
		
	}
	
	public void registerContainedBean(String containedBeanName, String containingBeanName) {
		
	}
	
	public void registerDependentBean(String beanName, String dependentBeanName) {
		
	}
	
	public String[] getDependentBeans(String beanName) {
		
	}
	
	public String[] getDependenciesForBean(String beanName) {
		
	}
	
	public void destroySingleton(String beanName) {
		
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
	
	protected void addSingletonFactory(String beanName, ObjectFactory singletonFactory) {
		
	}
	
	protected Object getSingleton(String beanName, boolean allowEarlyReference) {
		
	}
	
	protected void onSuppressedException(Exception ex) {
		
	}
	
	protected void removeSingleton(String beanName) {
		
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
	
	protected void destroyBean(String beanName, DisposableBean bean) {
		
	}
	
	/**
	 * Expose the singleton mutex to subclasses.
	 * @return
	 */
	protected final Object getSingletonMutex() {
		return singletonObjects;
	}
}
