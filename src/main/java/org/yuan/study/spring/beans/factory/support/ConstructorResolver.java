package org.yuan.study.spring.beans.factory.support;

import java.beans.ConstructorProperties;
import java.lang.reflect.Constructor;

import org.yuan.study.spring.beans.BeanWrapper;
import org.yuan.study.spring.util.ClassUtils;

public class ConstructorResolver {
	
	private static final String CONSTRUCTOR_PROPERTIES_CLASS_NAME = "java.beans.ConstructorProperties";
	
	private static final boolean CONSTRUCTOR_PROPERTIES_ANNOTATION_AVAILABLE = 
		ClassUtils.isPresent(CONSTRUCTOR_PROPERTIES_CLASS_NAME, ConstructorResolver.class.getClassLoader());

	private final AbstractAutowireCapableBeanFactory beanFactory;
	
	/**
	 * 
	 * @param beanFactory
	 */
	public ConstructorResolver(AbstractAutowireCapableBeanFactory beanFactory) {
		this.beanFactory = beanFactory;
	}

	/**
	 * 
	 * @param beanName
	 * @param mbd
	 * @param chosenCtors
	 * @param explicitArgs
	 * @return
	 */
	public BeanWrapper autowireConstructor(final String beanName, final RootBeanDefinition mbd, Constructor[] chosenCtors, final Object[] explicitArgs) {
		
	}
	
	public void resolveFactoryMethodIfPossible(RootBeanDefinition mbd) {
		
	}
	
	public BeanWrapper instantiateUsingFactoryMethod(final String beanName, final RootBeanDefinition mbd, final Object[] explicitArgs) {
		
	}
	
	private int resolveConstructorArguments() {
		
	}
	
	private ArgumentsHolder createArgumentArray() {
		
	}
	
	private Object[] resolvePreparedArguments() {
		
	}
	
	protected Object resolveAutowiredArgument() {
		
	}
	
	/**
	 * 
	 * @author Yuan
	 *
	 */
	private static class ArgumentsHolder {
		
		public final Object[] rawArguments;
		
		public final Object[] arguments;
		
		public final Object[] preparedArguments;
		
		public boolean resolveNecessary = false;
		
		public ArgumentsHolder(int size) {
			rawArguments = new Object[size];
			arguments = new Object[size];
			preparedArguments = new Object[size];
		}
		
		public ArgumentsHolder(Object[] args) {
			rawArguments = args;
			arguments = args;
			preparedArguments = args;
		}
		
		public int getTypeDifferenceWeight(Class<?>[] paramTypes) {
			int typeDiffWeight = MethodInvoker.getTypeDifferenceWeight(paramTypes, arguments);
			int rawTypeDiffWeight = MethodInvoker.getTypeDifferenceWeight(paramTypes, rawArguments) - 1024;
			return (rawTypeDiffWeight < typeDiffWeight ? rawTypeDiffWeight : typeDiffWeight);
		}
		
		public int getAssignabilityWeight(Class<?>[] paramTypes) {
			for (int i = 0; i < paramTypes.length; i++) {
				if (!ClassUtils.isAssignableValue(paramTypes[i], arguments[i])) {
					return Integer.MAX_VALUE;
				}
			}
			for (int i = 0; i < paramTypes.length; i++) {
				if (!ClassUtils.isAssignableValue(paramTypes[i], rawArguments[i])) {
					return Integer.MAX_VALUE - 512;
				}
			}
			return Integer.MAX_VALUE - 1024;
		}
		
		public void storeCache(RootBeanDefinition mbd, Object constructorOrFactoryMethod) {
			synchronized (mbd.constructorArgumentLock) {
				mbd.resolvedConstructorOrFactoryMethod = constructorOrFactoryMethod;
				mbd.constructorArgumentsResolved = true;
				if (resolveNecessary) {
					mbd.preparedConstructorArguments = preparedArguments;
				}
				else {
					mbd.resolvedConstructorArguments = arguments;
				}
			}
		}
	}
	
	/**
	 * Marker for autowired arguments in a cached argument array.
	 */
	private static class AutowiredArgumentMarker {
		
	}
	
	/**
	 * Inner class to avoid a Java 6 dependency.
	 */
	private static class ConstructorPropertiesChecker {
		
		public static String[] evaluateAnnotation(Constructor<?> candidate, int paramCount) {
			ConstructorProperties cp = candidate.getAnnotation(ConstructorProperties.class);
			if (cp != null) {
				String[] names = cp.value();
				if (names.length != paramCount) {
					throw new IllegalStateException(String.format(
						"Constructor annotated with @ConstructorProperties but not corresponding to actual number of parameters (%s): %s", 
							paramCount, candidate));
				}
				return names;
			}
			else {
				return null;
			}
		}
	}
}
