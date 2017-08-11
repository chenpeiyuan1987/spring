package org.yuan.study.spring.beans.factory.support;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

import net.sf.cglib.proxy.Callback;
import net.sf.cglib.proxy.CallbackFilter;
import net.sf.cglib.proxy.Enhancer;
import net.sf.cglib.proxy.MethodInterceptor;
import net.sf.cglib.proxy.MethodProxy;
import net.sf.cglib.proxy.NoOp;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.yuan.study.spring.beans.factory.BeanFactory;

public class CglibSubclassingInstantiationStrategy extends SimpleInstantiationStrategy {
	
	private static final int PASSTHROUGH = 0;
	
	private static final int LOOKUP_OVERRIDE = 1;
	
	private static final int METHOD_REPLACER = 2;

	//-----------------------------------------------------------------------------
	// Implementation of CglibSubclassingInstantiationStrategy interface
	//-----------------------------------------------------------------------------
	
	@Override
	protected Object instantiateWithMethodInjection(RootBeanDefinition beanDefinition, String beanName, BeanFactory owner) {
		return new CglibSubclassCreator(beanDefinition, owner).instantiate(null, null);
	}

	@Override
	protected Object instantiateWithMethodInjection(RootBeanDefinition beanDefinition, String beanName,BeanFactory owner, 
		Constructor<?> ctor, Object[] args) {
		return new CglibSubclassCreator(beanDefinition, owner).instantiate(ctor, args);
	}
	
	//------------------------------------------------------------------------------
	// Private inner class
	//------------------------------------------------------------------------------
	
	private static class CglibSubclassCreator {
		
		private static final Log logger = LogFactory.getLog(CglibSubclassingInstantiationStrategy.class);
		
		private final RootBeanDefinition beanDefinition;
		
		private final BeanFactory owner;

		public CglibSubclassCreator(RootBeanDefinition beanDefinition, BeanFactory owner) {
			this.beanDefinition = beanDefinition;
			this.owner = owner;
		}
		
		public Object instantiate(Constructor<?> ctor, Object[] args) {
			Enhancer enhancer = new Enhancer();
			enhancer.setSuperclass(this.beanDefinition.getBeanClass());
			enhancer.setCallbackFilter(new CallbackFilterImpl());
			enhancer.setCallbacks(new Callback[] {
				NoOp.INSTANCE,
				new LookupOverrideMethodInterceptor(),
				new ReplaceOverrideMethodInterceptor()
			});
			return (ctor == null) ? enhancer.create() : enhancer.create(ctor.getParameterTypes(), args);
		}
		
		private class CglibIdentitySupport {
			
			/**
			 * Exposed for equals method to allow access to enclosing class field.
			 * @return
			 */
			protected RootBeanDefinition getBeanDefinition() {
				return beanDefinition;
			}

			@Override
			public int hashCode() {
				return beanDefinition.hashCode();
			}

			@Override
			public boolean equals(Object other) {
				return (other.getClass() == getClass()) 
					&& ((CglibIdentitySupport)other).getBeanDefinition() == beanDefinition;
			}
			
		}
		
		/**
		 * 
		 * @author Yuan
		 *
		 */
		private class LookupOverrideMethodInterceptor extends CglibIdentitySupport implements MethodInterceptor {

			@Override
			public Object intercept(Object obj, Method method, Object[] args, MethodProxy proxy) throws Throwable {
				LookupOverride lookupOverride = (LookupOverride) beanDefinition.getMethodOverrides().getOverride(method);
				return owner.getBean(lookupOverride.getBeanName());
			}
			
		}
		
		/**
		 * CGLIB MethodInterceptor to override methods, replacing them with a call to a generic MethodReplacer.
		 */
		private class ReplaceOverrideMethodInterceptor extends CglibIdentitySupport implements MethodInterceptor {

			@Override
			public Object intercept(Object obj, Method method, Object[] args, MethodProxy proxy) throws Throwable {
				ReplaceOverride replaceOverride = (ReplaceOverride) beanDefinition.getMethodOverrides().getOverride(method);
				MethodReplacer methodReplacer = (MethodReplacer) owner.getBean(replaceOverride.getMethodReplacerBeanName());
				return methodReplacer.reimplement(obj, method, args);
			}
			
		}
		
		/**
		 * CGLIB object to filter method interception behavior.
		 */
		private class CallbackFilterImpl extends CglibIdentitySupport implements CallbackFilter {

			@Override
			public int accept(Method method) {
				MethodOverride methodOverride = beanDefinition.getMethodOverrides().getOverride(method);
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Override for '%s' is [%s]", method.getName(), methodOverride));
				}
				if (methodOverride == null) {
					return PASSTHROUGH;
				}
				if (methodOverride instanceof LookupOverride) {
					return LOOKUP_OVERRIDE;
				}
				if (methodOverride instanceof ReplaceOverride) {
					return METHOD_REPLACER;
				}
				throw new UnsupportedOperationException(String.format(
					"Unexpected MethodOverride subclass: %s", methodOverride.getClass().getName()));
			}
			
		}
	}
}
