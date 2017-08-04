package org.yuan.study.spring.beans;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.WeakHashMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

final class CachedIntrospectionResults {

	private static final Log logger = LogFactory.getLog(CachedIntrospectionResults.class);
	
	/** Map keyed by class containing CachedIntrospectionResults. */
	private static final Map<Class<?>, Object> classCache = Collections.synchronizedMap(new WeakHashMap<Class<?>, Object>());
	
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
			results = (CachedIntrospectionResults) value;
		}
		
		if (results == null) {
			results = new CachedIntrospectionResults(beanClass);
			boolean cacheSafe = isCacheSafe(beanClass);
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("Class [%s] is %scache-safe", beanClass.getName(), (cacheSafe ? "" : "not ")));
			}
			if (cacheSafe) {
				classCache.put(beanClass, results);
			} else {
				classCache.put(beanClass, new WeakReference<>(results));
			}
		}
		else {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("Using cached introspection results for class [%s]", beanClass.getName()));
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
	private CachedIntrospectionResults(Class<?> beanClass) throws BeansException {
		try {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("Getting BeanInfo for class [%s]", beanClass.getName()));
			}
			this.beanInfo = Introspector.getBeanInfo(beanClass);
			
			Class<?> classToFlush = beanClass;
			do {
				Introspector.flushFromCaches(classToFlush);
				classToFlush = classToFlush.getSuperclass();
			}
			while (classToFlush != null);
			
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("Caching PropertyDescriptor for class [%s]", beanClass.getName()));
			}
			this.propertyDescriptorCache = new HashMap<String,PropertyDescriptor>();
			PropertyDescriptor[] pds = this.beanInfo.getPropertyDescriptors();
			for (PropertyDescriptor pd : pds) {
				this.propertyDescriptorCache.put(pd.getName(), pd);
			}
		}
		catch (IntrospectionException ex) {
			throw new FatalBeanException(String.format("Cannot get BeanInfo for object of class [%s]", beanClass.getName()), ex);
		}
	}

	public BeanInfo getBeanInfo() {
		return beanInfo;
	}
	
	public Class<?> getBeanClass() {
		return this.beanInfo.getBeanDescriptor().getBeanClass();
	}
	
	public PropertyDescriptor getPropertyDescriptor(String propertyName) {
		return (PropertyDescriptor) this.propertyDescriptorCache.get(propertyName);
	}
}
