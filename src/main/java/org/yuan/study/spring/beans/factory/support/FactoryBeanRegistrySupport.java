package org.yuan.study.spring.beans.factory.support;

import java.security.AccessControlContext;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.factory.BeanCreationException;
import org.yuan.study.spring.beans.factory.BeanCurrentlyInCreationException;
import org.yuan.study.spring.beans.factory.FactoryBean;
import org.yuan.study.spring.beans.factory.FactoryBeanNotInitializedException;

public abstract class FactoryBeanRegistrySupport extends DefaultSingletonBeanRegistry {
	
	/** Cache of singleton objects created by FactoryBeans: FactoryBean name --> object */
	private final Map<String, Object> factoryBeanObjectCache = new ConcurrentHashMap<String, Object>();

	/**
	 * Determine the type for the given FactoryBean.
	 * @return
	 */
	protected Class<?> getTypeForFactoryBean(final FactoryBean factoryBean) {
		try {
			if (System.getSecurityManager() != null) {
				return AccessController.doPrivileged(new PrivilegedAction<Class<?>>() {
					@Override
					public Class<?> run() {
						return factoryBean.getObjectType();
					}
				}, getAccessControlContext());
			} else {
				return factoryBean.getObjectType();
			}
		} 
		catch (Throwable ex) {
			logger.warn("FactoryBean threw exception from getObjectType, despite the contract saying "
					+ "that it should return null if the type of its object cannot be determined yet", ex);
			return null;
		}
	}
	
	/**
	 * Obtain an object to expose from the given FactoryBean, if available
	 * in cached form. Quick check for minimal synchronization.
	 * @param beanName
	 * @return
	 */
	protected Object getCachedObjectForFactoryBean(String beanName) {
		Object object = factoryBeanObjectCache.get(beanName);
		return (object != NULL_OBJECT ? object : null);
	}
	
	/**
	 * Obtain an object to expose from the given FactoryBean.
	 * @param factory
	 * @param beanName
	 * @param shouldPostProcess
	 * @return
	 */
	protected Object getObjectFromFactoryBean(FactoryBean factory, String beanName, boolean shouldPostProcess) {
		if (factory.isSingleton() && containsSingleton(beanName)) {
			synchronized (getSingletonMutex()) {
				Object object = factoryBeanObjectCache.get(beanName);
				if (object == null) {
					object = doGetObjectFromFactoryBean(factory, beanName, shouldPostProcess);
					factoryBeanObjectCache.put(beanName, (object != null ? object : NULL_OBJECT));
				}
				return (object != NULL_OBJECT ? object : null);
			}
		} 
		else {
			return doGetObjectFromFactoryBean(factory, beanName, shouldPostProcess);
		}
	}
	
	/**
	 * Post-process the given object that has been obtained from the FactoryBean.
	 * @param object
	 * @param beanName
	 * @return
	 * @throws BeansException
	 */
	protected Object postProcessObjectFromFactoryBean(Object object, String beanName) throws BeansException {
		return object;
	}
	
	/**
	 * Get a FactoryBean for the given bean if possible.
	 * @param beanName
	 * @param beanInstance
	 * @return
	 * @throws BeansException
	 */
	protected FactoryBean getFactoryBean(String beanName, Object beanInstance) throws BeansException {
		if (!(beanInstance instanceof FactoryBean)) {
			throw new BeanCreationException(beanName, String.format(
				"Bean instance of type [%s] is not a FactoryBean", beanInstance.getClass()));
		}
		return (FactoryBean) beanInstance;
	}
	
	/**
	 * Overridden to clear the FactoryBean object cache as well.
	 */
	@Override
	protected void removeSingleton(String beanName) {
		super.removeSingleton(beanName);
		factoryBeanObjectCache.remove(beanName);
	}
	
	/**
	 * Returns the security context for this bean factory. if a security manager
	 * is set, interaction with the user code will be executed using the privileged
	 * of the security context returned by this method.
	 * @return
	 */
	protected AccessControlContext getAccessControlContext() {
		return AccessController.getContext();
	}
	
	/**
	 * Obtain an object to expose from the given FactoryBean.
	 * @return
	 */
	private Object doGetObjectFromFactoryBean(final FactoryBean factory, final String beanName, final boolean shouldPostProcess) throws BeanCreationException {
		Object object;
		try {
			if (System.getSecurityManager() != null) {
				AccessControlContext acc = getAccessControlContext();
				try {
					object = AccessController.doPrivileged(new PrivilegedExceptionAction<Object>() {
						@Override
						public Object run() throws Exception {
							return factory.getObject();
						}
					}, acc);
				} 
				catch (PrivilegedActionException pae) {
					throw pae.getException();
				}
			} 
			else {
				object = factory.getObject();
			}
		} 
		catch (FactoryBeanNotInitializedException ex) {
			throw new BeanCurrentlyInCreationException(beanName, ex.toString());
		}
		catch (Throwable ex) {
			throw new BeanCreationException(beanName, "FactoryBean threw exception on object creation", ex);
		}
		
		if (object == null && isSingletonCurrentlyInCreation(beanName)) {
			throw new BeanCurrentlyInCreationException(beanName, 
				"FactoryBean which is currently in creation returned null from getObject");
		}
		
		if (object != null && shouldPostProcess) {
			try {
				object = postProcessObjectFromFactoryBean(object, beanName);
			} 
			catch (Throwable ex) {
				throw new BeanCreationException(beanName, "Post processing of the FactoryBean's object failed", ex);
			}
		}
		
		return object;
	}
}
