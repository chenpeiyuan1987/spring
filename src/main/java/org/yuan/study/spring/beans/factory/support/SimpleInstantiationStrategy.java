package org.yuan.study.spring.beans.factory.support;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.util.StringUtils;
import org.yuan.study.spring.beans.factory.BeanDefinitionStoreException;
import org.yuan.study.spring.beans.factory.BeanFactory;

public class SimpleInstantiationStrategy implements InstantiationStrategy {
	
	protected final Log logger = LogFactory.getLog(getClass());
	
	
	//---------------------------------------------------------------------
	// Implementation of methods
	//---------------------------------------------------------------------

	/**
	 * Subclasses can override this method, which is implemented to throw UnsupportedOperationException, 
	 * if they can instantiate an object with the Method Injection specified in the given RootBeanDefinition.
	 * @param beanDefinition
	 * @param beanName
	 * @param owner
	 * @return
	 */
	protected Object instantiateWithMethodInjection(RootBeanDefinition beanDefinition, String beanName, BeanFactory owner) {
		throw new UnsupportedOperationException("Method Injection not supported in SimpleInstantiationStrategy");
	}
	
	/**
	 * Subclasses can override this method, which is implemented to throw UnsupportedOperationException, 
	 * if they can instantiate an object with the Method Injection specified in the given RootBeanDefinition.
	 * @param beanDefinition
	 * @param beanName
	 * @param owner
	 * @param ctor
	 * @param args
	 * @return
	 */
	protected Object instantiateWithMethodInjection(RootBeanDefinition beanDefinition, String beanName, BeanFactory owner, 
		Constructor<?> ctor, Object[] args) {
		throw new UnsupportedOperationException("Method Injection not supported in SimpleInstantiationStrategy");
	}
	
	//---------------------------------------------------------------------
	// Implementation of InstantiationStrategy interface
	//---------------------------------------------------------------------
	
	@Override
	public Object instantiate(RootBeanDefinition beanDefinition, String beanName, BeanFactory owner) {
		if (beanDefinition.getMethodOverrides().isEmpty()) {
			return BeanUtils.instantiateClass(beanDefinition.getBeanClass());
		}
		else {
			return instantiateWithMethodInjection(beanDefinition, beanName, owner);
		}
	}

	@Override
	public Object instantiate(RootBeanDefinition beanDefinition, String beanName, BeanFactory owner,
		Constructor<?> ctor, Object[] args) {
		if (beanDefinition.getMethodOverrides().isEmpty()) {
			return BeanUtils.instantiateClass(ctor, args);
		}
		else {
			return instantiateWithMethodInjection(beanDefinition, beanName, owner, ctor, args);
		}
	}

	@Override
	public Object instantiate(RootBeanDefinition beanDefinition, String beanName, BeanFactory owner,
		Object factoryBean, Method factoryMethod, Object[] args) {
		try {
			if (!Modifier.isPublic(factoryMethod.getModifiers()) 
				|| !Modifier.isPublic(factoryMethod.getDeclaringClass().getModifiers())) {
				factoryMethod.setAccessible(true);
			}
			return factoryMethod.invoke(factoryBean, args);
		} 
		catch (IllegalArgumentException ex) {
			throw new BeanDefinitionStoreException(String.format(
				"Illegal arguments to factory method [%s]; args: %s", factoryMethod, StringUtils.arrayToCommaDelimitedString(args)));
		}
		catch (IllegalAccessException ex) {
			throw new BeanDefinitionStoreException(String.format(
				"Cannot access factory method [%s]; is it public?", factoryMethod));
		}
		catch (InvocationTargetException ex) {
			String msg = String.format("Factory method [%s] threw exception", factoryMethod);
			logger.warn(msg, ex.getTargetException());
			throw new BeanDefinitionStoreException(msg, ex.getTargetException());
		}
	}

}
