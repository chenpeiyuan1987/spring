package org.yuan.study.spring.beans.factory.support;

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.security.AccessControlContext;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.yuan.study.spring.beans.BeanUtils;
import org.yuan.study.spring.beans.factory.DisposableBean;
import org.yuan.study.spring.beans.factory.config.BeanPostProcessor;
import org.yuan.study.spring.beans.factory.config.DestructionAwareBeanPostProcessor;
import org.yuan.study.spring.util.Assert;
import org.yuan.study.spring.util.ReflectionUtils;

public class DisposableBeanAdapter implements Runnable, DisposableBean, Serializable {
	private static final long serialVersionUID = 1L;

	private static final Log logger = LogFactory.getLog(DisposableBeanAdapter.class);
	
	private final Object bean;
	
	private final String beanName;
	
	private final boolean invokeDisposableBean;
	
	private final boolean nonPublicAccessAllowed;
	
	private String destroyMethodName;
	
	private transient Method destroyMethod;
	
	private List<DestructionAwareBeanPostProcessor> beanPostProcessors;
	
	private final AccessControlContext acc;
	
	
	public DisposableBeanAdapter(Object bean, String beanName, RootBeanDefinition beanDefinition,
		List<BeanPostProcessor> postProcessors, AccessControlContext acc) {
		Assert.notNull(bean, "Disposable bean must not be null");
		
		this.bean = bean;
		this.beanName = beanName;
		this.invokeDisposableBean = (this.bean instanceof DisposableBean && !beanDefinition.isExternallyManagedDestroyMethod("destroy"));
		this.nonPublicAccessAllowed = beanDefinition.isNonPublicAccessAllowed();
		this.acc = acc;
		
		final String destroyMethodName = beanDefinition.getDestroyMethodName();
		if (destroyMethodName != null && !(this.invokeDisposableBean && "destroy".equals(destroyMethodName)) 
			&& !beanDefinition.isExternallyManagedDestroyMethod(destroyMethodName)) {
			this.destroyMethodName = destroyMethodName;
			this.destroyMethod = determineDestroyMethod();
			if (this.destroyMethod == null) {
				if (beanDefinition.isEnforceDestroyMethod()) {
					throw new BeanDefinitionValidationException(String.format(
						"Couldn't find a destroy method named '%s' on bean with name '%s'", 
							destroyMethodName, beanName));
				}
			}
			else {
				Class<?>[] paramTypes = destroyMethod.getParameterTypes();
				if (paramTypes.length > 1) {
					throw new BeanDefinitionValidationException(String.format(
						"Method '%s' of bean '%s' has more than one parameter - "
						+ "not supported as destroy method", destroyMethodName, beanName));
				} 
				else if (paramTypes.length == 1 && !paramTypes[0].equals(boolean.class)) {
					throw new BeanDefinitionValidationException(String.format(
						"Method '%s' of bean '%s' has a non-boolean parameter - "
						+ "not supported as destroy method", destroyMethodName, beanName));
				}
			}
		}
		this.beanPostProcessors = filterPostProcessors(postProcessors);
	}

	/**
	 * Create a new DisposableBeanAdapter for the given bean.
	 * @param bean
	 * @param beanName
	 * @param invokeDisposableBean
	 * @param nonPublicAccessAllowed
	 * @param destroyMethodName
	 * @param beanPostProcessors
	 */
	private DisposableBeanAdapter(Object bean, String beanName, boolean invokeDisposableBean,
		boolean nonPublicAccessAllowed, String destroyMethodName, List<DestructionAwareBeanPostProcessor> postProcessors) {
		this.bean = bean;
		this.beanName = beanName;
		this.invokeDisposableBean = invokeDisposableBean;
		this.nonPublicAccessAllowed = nonPublicAccessAllowed;
		this.destroyMethodName = destroyMethodName;
		this.beanPostProcessors = postProcessors;
		this.acc = null;
	}
	
	/**
	 * Search for all DestructionAwareBeanPostProcessors in the List.
	 * @param postProcessors
	 * @return
	 */
	private List<DestructionAwareBeanPostProcessor> filterPostProcessors(List<BeanPostProcessor> postProcessors) {
		List<DestructionAwareBeanPostProcessor> filteredPostProcessors = null;
		if (postProcessors != null && !postProcessors.isEmpty()) {
			filteredPostProcessors = new ArrayList<DestructionAwareBeanPostProcessor>(postProcessors.size());
			for (BeanPostProcessor postProcessor : postProcessors) {
				if (postProcessor instanceof DestructionAwareBeanPostProcessor) {
					filteredPostProcessors.add((DestructionAwareBeanPostProcessor) postProcessor);
				}
			}
		}
		return filteredPostProcessors;
	}
	
	private Method determineDestroyMethod() {
		try {
			if (System.getSecurityManager() != null) {
				return AccessController.doPrivileged(new PrivilegedAction<Method>() {
					@Override
					public Method run() {
						return findDestroyMethod();
					}
				});
			} 
			else {
				return findDestroyMethod();
			}
		} 
		catch (IllegalArgumentException ex) {
			throw new BeanDefinitionValidationException(String.format(
				"Couldn't find a unique destroy method on bean with name '%s': %s", beanName, ex.getMessage()));
		}
	}
	
	private Method findDestroyMethod() {
		return nonPublicAccessAllowed 
			? BeanUtils.findMethodWithMinimalParameters(bean.getClass(), destroyMethodName)
			: BeanUtils.findMethodWithMinimalParameters(bean.getClass().getMethods(), destroyMethodName);
	}
	
	/**
	 * Invoke the specified custom destroy method on the given bean.
	 * @param destroyMethod
	 */
	private void invokeCustomDestroyMethod(final Method destroyMethod) {
		Class<?>[] paramTypes = destroyMethod.getParameterTypes();
		final Object[] args = new Object[paramTypes.length];
		if (paramTypes.length == 1) {
			args[0] = Boolean.TRUE;
		}
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Invoking destroy method '%s' on bean with name '%s'", 
				destroyMethodName, beanName));
		}
		try {
			if (System.getSecurityManager() != null) {
				AccessController.doPrivileged(new PrivilegedAction<Object>() {
					@Override
					public Object run() {
						ReflectionUtils.makeAccessible(destroyMethod);
						return null;
					}
				});
				try {
					AccessController.doPrivileged(new PrivilegedExceptionAction<Object>() {
						@Override
						public Object run() throws Exception {
							destroyMethod.invoke(bean, args);
							return null;
						}
					}, acc);
				} 
				catch (PrivilegedActionException pax) {
					throw (InvocationTargetException) pax.getException();
				}
			} 
			else {
				ReflectionUtils.makeAccessible(destroyMethod);
				destroyMethod.invoke(bean, args);
			}
		} 
		catch (InvocationTargetException ex) {
			String msg = String.format("Invocation of destroy method '%s' failed on bean with name '%s'", 
				destroyMethodName, beanName);
			if (logger.isDebugEnabled()) {
				logger.warn(msg, ex.getTargetException());
			} 
			else {
				logger.warn(msg + ": " + ex.getTargetException());
			}
		}
		catch (Throwable ex) {
			logger.error(String.format("Couln't invoke destory method '%s' on bean with name '%s'", 
				destroyMethodName, beanName));
		}
	}
	
	/**
	 * Serializes a copy of the state of this class, 
	 * filtering out non serializable BeanPostProcessors.
	 * @return
	 */
	protected Object writeReplace() {
		List<DestructionAwareBeanPostProcessor> serializablePostProcessors = null;
		if (beanPostProcessors != null) {
			serializablePostProcessors = new ArrayList<DestructionAwareBeanPostProcessor>();
			for (DestructionAwareBeanPostProcessor postProcessor : beanPostProcessors) {
				if (postProcessor instanceof Serializable) {
					serializablePostProcessors.add(postProcessor);
				}
			}
		}
		return new DisposableBeanAdapter(bean, beanName, invokeDisposableBean, 
			nonPublicAccessAllowed, destroyMethodName, serializablePostProcessors);
	}

	@Override
 	public void destroy() {
		if (beanPostProcessors != null && !beanPostProcessors.isEmpty()) {
			for (DestructionAwareBeanPostProcessor processor : beanPostProcessors) {
				processor.postProcessBeforeDestruction(bean, beanName);
			}
		}
		
		if (invokeDisposableBean) {
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("Invoking destroy() on bean with name '%s'", beanName));
			}
			
			try {
				if (System.getSecurityManager() != null) {
					AccessController.doPrivileged(new PrivilegedExceptionAction<Object>() {
						@Override
						public Object run() throws Exception {
							((DisposableBean) bean).destroy();
							return null;
						}
					}, acc);
				}
				else {
					((DisposableBean) bean).destroy();
				}
			} 
			catch (Throwable ex) {
				String msg = String.format("Invocation of destroy method failed on bean with name '%s'", beanName);
				if (logger.isDebugEnabled()) {
					logger.warn(msg, ex);
				} 
				else {
					logger.warn(msg + ": " + ex);
				}
			}
		}
		
		if (destroyMethod != null) {
			invokeCustomDestroyMethod(destroyMethod);
		} 
		else if (destroyMethodName != null) {
			Method methodToCall = determineDestroyMethod();
			if (methodToCall != null) {
				invokeCustomDestroyMethod(methodToCall);
			}
		}
	}

	@Override
	public void run() {
		destroy();
	}

}
