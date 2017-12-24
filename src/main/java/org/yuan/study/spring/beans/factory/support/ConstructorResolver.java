package org.yuan.study.spring.beans.factory.support;

import java.beans.ConstructorProperties;
import java.lang.reflect.Constructor;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Set;

import org.yuan.study.spring.beans.BeanMetadataElement;
import org.yuan.study.spring.beans.BeanWrapper;
import org.yuan.study.spring.beans.TypeConverter;
import org.yuan.study.spring.beans.TypeMismatchException;
import org.yuan.study.spring.beans.factory.UnsatisfiedDependencyException;
import org.yuan.study.spring.beans.factory.config.ConstructorArgumentValues;
import org.yuan.study.spring.beans.factory.config.DependencyDescriptor;
import org.yuan.study.spring.core.GenericTypeResolver;
import org.yuan.study.spring.core.MethodParameter;
import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.ObjectUtils;
import org.yuan.study.spring.util.ReflectionUtils;

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
	
	/**
	 * Resolve the factory method in the specified bean definition, if possible.
	 * @param mbd
	 */
	public void resolveFactoryMethodIfPossible(RootBeanDefinition mbd) {
		Class<?> factoryClass;
		if (mbd.getFactoryBeanName() != null) {
			factoryClass = beanFactory.getType(mbd.getFactoryBeanName());
		}
		else {
			factoryClass = mbd.getBeanClass();
		}
		
		factoryClass = ClassUtils.getUserClass(factoryClass);
		Method[] candidates = ReflectionUtils.getAllDeclaredMethods(factoryClass);
		Method uniqueCandidate = null;
		for (Method candidate : candidates) {
			if (mbd.isFactoryMethod(candidate)) {
				if (uniqueCandidate == null) {
					uniqueCandidate = candidate;
				} 
				else if (!Arrays.equals(uniqueCandidate.getParameterTypes(), candidate.getParameterTypes())) {
					uniqueCandidate = null;
					break;
				}
			}
		}
		synchronized (mbd.constructorArgumentLock) {
			mbd.resolvedConstructorOrFactoryMethod = uniqueCandidate;
		}
	}
	
	public BeanWrapper instantiateUsingFactoryMethod(final String beanName, final RootBeanDefinition mbd, final Object[] explicitArgs) {
		
	}
	
	/**
	 * Resolve the constructor arguments for this bean into the resolvedValues object.
	 * @param beanName
	 * @param mbd
	 * @param bw
	 * @param cargs
	 * @param resolvedValues
	 * @return
	 */
	private int resolveConstructorArguments(String beanName, RootBeanDefinition mbd, BeanWrapper bw, ConstructorArgumentValues cargs, 
		ConstructorArgumentValues resolvedValues) {
		
	}
	
	/**
	 * Create an array of arguments to invoke a constructor or factory method,
	 * given the resolved constructor argument values.
	 * @return
	 */
	private ArgumentsHolder createArgumentArray(String beanName, RootBeanDefinition mbd, ConstructorArgumentValues resolvedValues,
		BeanWrapper bw, Class<?>[] paramTypes, String[] paramNames, Object methodOrCtor, boolean autowiring) throws UnsatisfiedDependencyException {
		
	}
	
	/**
	 * Resolve the prepared arguments stored in the given bean definition.
	 * @return
	 */
	private Object[] resolvePreparedArguments(String beanName, RootBeanDefinition mbd, BeanWrapper bw, Member methodOrCtor, Object[] argsToResolve) {
		Class<?>[] paramTypes = (methodOrCtor instanceof Method 
			? ((Method) methodOrCtor).getParameterTypes() : ((Constructor<?>) methodOrCtor).getParameterTypes());
		TypeConverter converter = (beanFactory.getCustomTypeConverter() != null ? beanFactory.getCustomTypeConverter() : bw);
		BeanDefinitionValueResolver valueResolver = new BeanDefinitionValueResolver(beanFactory, beanName, mbd, converter);
		Object[] resolvedArgs = new Object[argsToResolve.length];
		for (int argIndex = 0; argIndex < argsToResolve.length; argIndex++) {
			Object argValue = argsToResolve[argIndex];
			MethodParameter methodParam = MethodParameter.forMethodOrConstructor(methodOrCtor, argIndex);
			GenericTypeResolver.resolveParameterType(methodParam, methodOrCtor.getDeclaringClass());
			if (argValue instanceof AutowiredArgumentMarker) {
				argValue = resolveAutowiredArgument(methodParam, beanName, null, converter);
			}
			else if (argValue instanceof BeanMetadataElement) {
				argValue = valueResolver.resolveValueIfNecessary("constructor argument", argValue);
			}
			else if (argValue instanceof String) {
				argValue = beanFactory.evaluateBeanDefinitionString((String) argValue, mbd);
			}
			
			Class<?> paramType = paramTypes[argIndex];
			try {
				resolvedArgs[argIndex] = converter.convertIfNecessary(argValue, paramType, methodParam);
			} 
			catch (TypeMismatchException ex) {
				String methodType = (methodOrCtor instanceof Constructor ? "constructor" : "factory method");
				throw new UnsatisfiedDependencyException(mbd.getResourceDescription(), beanName, argIndex, paramType, 
					String.format("Could not convert %s argument value of type [%s] to required type [%s]: %s", 
						methodType, ObjectUtils.nullSafeClassName(argValue), paramType.getName(), ex.getMessage()));
			}
		}
		return resolvedArgs;
	}
	
	/**
	 * Template method for resolving the specified argument which is supposed to be autowired.
	 * @return
	 */
	protected Object resolveAutowiredArgument(MethodParameter param, String beanName, Set<String> autowiredBeanNames, TypeConverter typeConverter) {
		return beanFactory.resolveDependency(new DependencyDescriptor(param, true), beanName, autowiredBeanNames, typeConverter);
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
