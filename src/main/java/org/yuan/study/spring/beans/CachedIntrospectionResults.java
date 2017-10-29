package org.yuan.study.spring.beans;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.StringUtils;

final class CachedIntrospectionResults {

	private static final Log logger = LogFactory.getLog(CachedIntrospectionResults.class);
	
	/** 
	 * Map keyed by class containing CachedIntrospectionResults. 
	 * Needs to be WeakHashMap with WeakReferences as values to allow
	 * for proper garbage collection in case of multiple class loaders.
	 */
	static final Map<Class<?>, Object> classCache = Collections.synchronizedMap(new WeakHashMap<Class<?>, Object>());
	
	/**
	 * Set of ClassLoaders that this CachedIntrospectionResults class will always 
	 * accept classes from, even if the classes do not qualify as cache-safe.
	 */
	static final Set<ClassLoader> acceptedClassLoaders = Collections.synchronizedSet(new HashSet<ClassLoader>());
	
	/**
	 * Accept the given ClassLoader as cache-safe, even if its classes would not qualify
	 * as cache-safe in this CachedIntrospectionResults class.
	 * @param classLoader
	 */
	public static void acceptClassLoader(ClassLoader classLoader) {
		if (classLoader != null) {
			acceptedClassLoaders.add(classLoader);
		}
	}
	
	/**
	 * 
	 * @param classLoader
	 */
	public static void clearClassLoader(ClassLoader classLoader) {
		if (classLoader == null) {
			return;
		}
		
		synchronized (classCache) {
			for (Iterator<Class<?>> iterator = classCache.keySet().iterator(); iterator.hasNext();) {
				Class<?> beanClass = iterator.next();
				if (isUnderneathClassLoader(beanClass.getClassLoader(), classLoader)) {
					iterator.remove();
				}
			}
		}
		
		synchronized (acceptedClassLoaders) {
			for (Iterator<ClassLoader> iterator = acceptedClassLoaders.iterator(); iterator.hasNext();) {
				ClassLoader registeredLoader = iterator.next();
				if (isUnderneathClassLoader(registeredLoader, classLoader)) {
					iterator.remove();
				}
			}
		}
	}
	
	/**
	 * 
	 * @param classLoader
	 * @return
	 */
	private static boolean isClassLoaderAccepted(ClassLoader classLoader) {
		ClassLoader[] acceptedLoaderArray = acceptedClassLoaders.toArray(new ClassLoader[acceptedClassLoaders.size()]);
		for (ClassLoader registeredLoader : acceptedLoaderArray) {
			if (isUnderneathClassLoader(classLoader, registeredLoader)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * 
	 * @param candidate
	 * @param parent
	 * @return
	 */
	private static boolean isUnderneathClassLoader(ClassLoader candidate, ClassLoader parent) {
		if (candidate == null) {
			return false;
		}
		if (candidate == parent) {
			return true;
		}
		ClassLoader classLoaderToCheck = candidate;
		while (classLoaderToCheck != null) {
			classLoaderToCheck = classLoaderToCheck.getParent();
			if (classLoaderToCheck == parent) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Create CachedIntrospectionResults for the given bean class.
	 * @param beanClass
	 * @return
	 * @throws BeansException
	 */
	public static CachedIntrospectionResults forClass(Class<?> beanClass) throws BeansException {
		CachedIntrospectionResults results = null;
		Object value = classCache.get(beanClass);
		if (value instanceof Reference) {
			Reference<?> reference = (Reference<?>) value;
			results = (CachedIntrospectionResults)reference.get();
		}
		else {
			results = (CachedIntrospectionResults)value;
		}
		
		if (results == null) {
			boolean fullyCacheable = ClassUtils.isCacheSafe(beanClass, CachedIntrospectionResults.class.getClassLoader()) 
				|| isClassLoaderAccepted(beanClass.getClassLoader());
			if (fullyCacheable || !ClassUtils.isPresent(beanClass.getName() + "BeanInfo", beanClass.getClassLoader())) {
				results = new CachedIntrospectionResults(beanClass, fullyCacheable);
				classCache.put(beanClass, results);
			}
			else {
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Not strongly caching class [%s] because it is not cache-safe", beanClass.getName()));
				}
				results = new CachedIntrospectionResults(beanClass, true);
				classCache.put(beanClass, new WeakReference<CachedIntrospectionResults>(results));
			}
		}
		
		return results;
	}
	
	/**
	 * Check whether the given class is cache-safe,
	 * i.e. whether it is loaded by the same class loader as the 
	 * CachedIntrospectionResults class or a parent of it.
	 * @param clazz
	 * @return
	 */
	public static boolean isCacheSafe(Class<?> clazz) {
		ClassLoader target = clazz.getClassLoader();
		if (target == null) {
			return false;
		}
		
		ClassLoader current = CachedIntrospectionResults.class.getClassLoader();
		if (current == target) {
			return true;
		}
		
		while (current != null) {
			current = current.getParent();
			if (current == target) {
				return true;
			}
		}
		return false;
	}
	
	
	//----------------------------------------------------
	// CachedIntrospectionResults members
	//----------------------------------------------------
	
	/** The BeanInfo object for the introspected bean class */
	private final BeanInfo beanInfo;
	
	/** PropertyDescriptor objects keyed by property name String */
	private final Map<String,PropertyDescriptor> propertyDescriptorCache;
	
	/**
	 * Create a new CachedIntrospectionResults instance for the given class.
	 * @param beanClass
	 * @throws BeansException
	 */
	private CachedIntrospectionResults(Class<?> beanClass, boolean cacheFullMetadata) throws BeansException {
		try {
			if (logger.isTraceEnabled()) {
				logger.trace(String.format("Getting BeanInfo for class [%s]", beanClass.getName()));
			}
			this.beanInfo = Introspector.getBeanInfo(beanClass);
			
			Class<?> classToFlush = beanClass;
			do {
				Introspector.flushFromCaches(classToFlush);
				classToFlush = classToFlush.getSuperclass();
			}
			while (classToFlush != null);
			
			if (logger.isTraceEnabled()) {
				logger.trace(String.format("Caching PropertyDescriptor for class [%s]", beanClass.getName()));
			}
			this.propertyDescriptorCache = new LinkedHashMap<String,PropertyDescriptor>();
			
			PropertyDescriptor[] pds = this.beanInfo.getPropertyDescriptors();
			for (PropertyDescriptor pd : pds) {
				if (Class.class.equals(beanClass) && "classLoader".equals(pd.getName())) {
					continue;
				}
				if (logger.isTraceEnabled()) {
					logger.trace(String.format("Found bean property '%s'%s%s", pd.getName(), 
						(pd.getPropertyType() != null ? " of type [" + pd.getPropertyType() + "]" : ""), 
						(pd.getPropertyEditorClass() != null ? "; editor [" + pd.getPropertyEditorClass().getName() + "]" : "")));
				}
				if (cacheFullMetadata) {
					pd = buildGenericTypeAwarePropertyDescriptor(beanClass, pd);
				}
				this.propertyDescriptorCache.put(pd.getName(), pd);
			}
		}
		catch (IntrospectionException ex) {
			throw new FatalBeanException(String.format("Cannot get BeanInfo for object of class [%s]", beanClass.getName()), ex);
		}
	}

	BeanInfo getBeanInfo() {
		return beanInfo;
	}
	
	Class<?> getBeanClass() {
		return this.beanInfo.getBeanDescriptor().getBeanClass();
	}
	
	PropertyDescriptor getPropertyDescriptor(String propertyName) {
		PropertyDescriptor pd = propertyDescriptorCache.get(propertyName);
		if (pd == null && StringUtils.hasLength(propertyName)) {
			pd = propertyDescriptorCache.get(propertyName.substring(0, 1).toLowerCase() + propertyName.substring(1));
			if (pd == null) {
				pd = propertyDescriptorCache.get(propertyName.substring(0, 1).toUpperCase() + propertyName.substring(1));
			}
		}
		return (pd == null || pd instanceof GenericTypeAwarePropertyDescriptor ? pd : 
			buildGenericTypeAwarePropertyDescriptor(getBeanClass(), pd));
	}
	
	PropertyDescriptor[] getPropertyDescriptors() {
		PropertyDescriptor[] pds = new PropertyDescriptor[propertyDescriptorCache.size()];
		int i = 0;
		for (PropertyDescriptor pd : propertyDescriptorCache.values()) {
			pds[i] = (pd instanceof GenericTypeAwarePropertyDescriptor ? 
				pd : buildGenericTypeAwarePropertyDescriptor(getBeanClass(), pd));
			i++;
		}
		return pds;
	}
	
	private PropertyDescriptor buildGenericTypeAwarePropertyDescriptor(Class<?> beanClass, PropertyDescriptor pd) {
		try {
			return new GenericTypeAwarePropertyDescriptor(beanClass, pd.getName(), pd.getReadMethod(), 
				pd.getWriteMethod(), pd.getPropertyEditorClass());
		}
		catch (IntrospectionException ex) {
			throw new FatalBeanException(String.format("Failed to re-introspect class [%s]", beanClass.getName()), ex);
		}
	}
}
