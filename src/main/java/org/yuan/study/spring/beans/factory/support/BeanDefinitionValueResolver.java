package org.yuan.study.spring.beans.factory.support;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.yuan.study.spring.beans.BeanWrapper;
import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.TypeConverter;
import org.yuan.study.spring.beans.factory.BeanCreationException;
import org.yuan.study.spring.beans.factory.BeanDefinitionStoreException;
import org.yuan.study.spring.beans.factory.BeanFactoryUtils;
import org.yuan.study.spring.beans.factory.FactoryBean;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.beans.factory.config.BeanDefinitionHolder;
import org.yuan.study.spring.beans.factory.config.RuntimeBeanNameReference;
import org.yuan.study.spring.beans.factory.config.RuntimeBeanReference;
import org.yuan.study.spring.beans.factory.config.TypedStringValue;
import org.yuan.study.spring.util.ClassUtils;
import org.yuan.study.spring.util.StringUtils;

public class BeanDefinitionValueResolver {
	
	protected static final Log logger = LogFactory.getLog(BeanDefinitionValueResolver.class);

	private final AbstractBeanFactory beanFactory;
	
	private final String beanName;
	
	private final BeanDefinition beanDefinition;
	
	private final TypeConverter typeConverter;
	
	/**
	 * Create a new BeanDefinitionValueResolver for the given BeanFactory and BeanDefinition.
	 * @param beanFactory
	 * @param beanName
	 * @param beanDefinition
	 */
	public BeanDefinitionValueResolver(AbstractBeanFactory beanFactory, String beanName, BeanDefinition beanDefinition, TypeConverter typeConverter) {
		this.beanName = beanName;
		this.beanFactory = beanFactory;
		this.beanDefinition = beanDefinition;
		this.typeConverter = typeConverter;
	}
	
	/**
	 * Given a PropertyValue, return a value, resolving any references to other beans in the factory if necessary.
	 * @param argName
	 * @param value
	 * @return
	 */
	public Object resolveValueIfNecessary(Object argName, Object value) {
		if (value instanceof RuntimeBeanReference) {
			RuntimeBeanReference runtimeBeanReference = (RuntimeBeanReference) value;
			return resolveReference(argName, runtimeBeanReference);
		}
		if (value instanceof RuntimeBeanNameReference) {
			String beanNameReference = ((RuntimeBeanNameReference) value).getBeanName();
			beanNameReference = String.valueOf(evaluate(beanNameReference));
			if (!beanFactory.containsBean(beanNameReference)) {
				throw new BeanDefinitionStoreException(String.format(
					"Invalid bean name '%s' in bean reference for %s", beanNameReference, argName));
			}
			return beanNameReference;
		}
		if (value instanceof BeanDefinitionHolder) {
			BeanDefinitionHolder beanDefinitionHolder = (BeanDefinitionHolder) value;
			return resolveInnerBean(argName, beanDefinitionHolder.getBeanName(), beanDefinitionHolder.getBeanDefinition());
		}
		if (value instanceof BeanDefinition) {
			BeanDefinition beanDefinition = (BeanDefinition) value;
			return resolveInnerBean(argName, "(inner bean)", beanDefinition);
		}
		if (value instanceof ManagedArray) {
			ManagedArray array = (ManagedArray) value;
			Class<?> elementType = array.resolvedElementType;
			if (elementType == null) {
				String elementTypeName = array.getElementTypeName();
				if (StringUtils.hasText(elementTypeName)) {
					try {
						elementType = ClassUtils.forName(elementTypeName, beanFactory.getBeanClassLoader());
						array.resolvedElementType = elementType;
					} 
					catch (Throwable ex) {
						throw new BeanCreationException(
							beanDefinition.getResourceDescription(), beanName, "" + argName, ex);
					}
				} 
				else {
					elementType = Object.class;
				}
			}
			return resolveManagedArray(argName, (List<?>) value, elementType);
		}
		if (value instanceof ManagedList) {
			return resolveManagedList(argName, (List<?>)value);
		}
		if (value instanceof ManagedSet) {
			return resolveManagedSet(argName, (Set<?>)value);
		}
		if (value instanceof ManagedMap) {
			return resolveManagedMap(argName, (Map<?,?>)value);
		}
		if (value instanceof ManagedProperties) {
			Properties original = (Properties) value;
			Properties copy = new Properties();
			for (Entry<Object, Object> entry : original.entrySet()) {
				Object key = entry.getKey();
				Object val = entry.getValue();
				if (key instanceof TypedStringValue) {
					key = evaluate((TypedStringValue) key);
				}
				if (val instanceof TypedStringValue) {
					val = evaluate((TypedStringValue) val);
				}
				copy.put(key, val);
			}
			return copy;
		}
		if (value instanceof TypedStringValue) {
			TypedStringValue typedStringValue = (TypedStringValue) value;
			Object valueObject = evaluate(typedStringValue);
			try {
				Class<?> resolvedTargetType = resolveTargetType(typedStringValue);
				if (resolvedTargetType != null) {
					return typeConverter.convertIfNecessary(valueObject, resolvedTargetType);
				}
				else {
					return valueObject;
				}
			}
			catch (Throwable ex) {
				throw new BeanCreationException(
					beanDefinition.getResourceDescription(), this.beanName, 
					"Error converting typed String value for" + argName, ex);
			}
		}
		
		return evaluate(value);
	}
	
	/**
	 * Checks the given bean name whether it is unique. If not already unique,
	 * a counter is added, increasing the counter until the name is unique.
	 * @param innerBeanName
	 * @return
	 */
	private String adaptInnerBeanName(String innerBeanName) {
		String actualInnerBeanName = innerBeanName;
		int counter = 0;
		while (beanFactory.isBeanNameInUse(actualInnerBeanName)) {
			counter++;
			actualInnerBeanName = innerBeanName + BeanFactoryUtils.GENERATED_BEAN_NAME_SEPARATOR + counter;
		}
		return actualInnerBeanName;
	}
	
	/**
	 * Resolve a reference to another bean in the factory.
	 * @param argName
	 * @param runtimeBeanReference
	 * @return
	 * @throws BeansException
	 */
	private Object resolveReference(Object argName, RuntimeBeanReference runtimeBeanReference) {
		try {
			String refName = runtimeBeanReference.getBeanName();
			refName = String.valueOf(evaluate(refName));
			if (runtimeBeanReference.isToParent()) {
				if (this.beanFactory.getParentBeanFactory() == null) {
					throw new BeanCreationException(this.beanDefinition.getResourceDescription(), this.beanName, 
						String.format("Can't resolve reference to bean '%s' in parent factory: no parent factory available", 
							runtimeBeanReference.getBeanName()));
				}
				return this.beanFactory.getParentBeanFactory().getBean(runtimeBeanReference.getBeanName());
			}
			else {
				Object bean = this.beanFactory.getBean(refName);
				this.beanFactory.registerDependentBean(refName, this.beanName);
				return bean;
			}
		}
		catch (BeansException ex) {
			throw new BeanCreationException(this.beanDefinition.getResourceDescription(), this.beanName, 
				String.format("Cannot resolve reference to bean '%s' while setting %s", runtimeBeanReference.getBeanName(), argName), ex);
		}
	}
	
	/**
	 * For each element in the managed array, resolve reference if necessary.
	 * @param argName
	 * @param list
	 * @param elementType
	 * @return
	 * @throws BeansException
	 */
	private Object resolveManagedArray(Object argName, List<?> list, Class<?> elementType) throws BeansException {
		Object resolved = Array.newInstance(elementType, list.size());
		for (int i=0; i<list.size(); i++) {
			Array.set(resolved, i, resolveValueIfNecessary(new KeyedArgName(argName, i), list.get(i)));
		}
		return resolved;
	}
	
	/**
	 * For each element in the managed list, resolve reference if necessary.
	 * @param argName
	 * @param list
	 * @return
	 * @throws BeansException
	 */
	private List<?> resolveManagedList(Object argName, List<?> list) throws BeansException {
		List<Object> resolved = new ArrayList<Object>(list.size());
		for (int i=0; i<list.size(); i++) {
			resolved.add(resolveValueIfNecessary(new KeyedArgName(argName, i), list.get(i)));
		}
		return resolved;
	}
	
	/**
	 * For each element in the managed set, resolve reference if necessary.
	 * @param argName
	 * @param set
	 * @return
	 * @throws BeansException
	 */
	private Set<?> resolveManagedSet(Object argName, Set<?> set) throws BeansException {
		Set<Object> resolved = new LinkedHashSet<Object>(set.size());
		int i = 0;
		for (Object object : set) {
			resolved.add(resolveValueIfNecessary(new KeyedArgName(argName, i), object));
			i++;
		}
		return resolved;
	}
	
	/**
	 * For each element in the managed map, resolve reference if necessary.
	 * @param argName
	 * @param map
	 * @return
	 * @throws BeansException
	 */
	private Map<?,?> resolveManagedMap(Object argName, Map<?,?> map) throws BeansException {
		Map<Object,Object> resolved = new LinkedHashMap<Object,Object>();
		for (Entry<?,?> entry : map.entrySet()) {
			Object resolvedKey = resolveValueIfNecessary(argName, entry.getKey());
			Object resolvedVal = resolveValueIfNecessary(
					new KeyedArgName(argName, entry.getKey()), entry.getValue());
			resolved.put(resolvedKey, resolvedVal);
		}
		return resolved;
	}

	/**
	 * Evaluate the given value as an expression, if necessary.
	 * @param value
	 * @return
	 */
	protected Object evaluate(Object value) {
		if (value instanceof String) {
			return beanFactory.evaluateBeanDefinitionString((String) value, beanDefinition);
		} 
		else {
			return value;
		}
	}
	
	/**
	 * Evaluate the given value as an expression, if necessary.
	 * @param value
	 * @return
	 */
	protected Object evaluate(TypedStringValue value) {
		Object result = beanFactory.evaluateBeanDefinitionString(value.getValue(), beanDefinition);
		if (result != value.getValue()) {
			value.setDynamic();
		}
		return result;
	}
	
	/**
	 * Resolve the target type in the given TypedStringValue.
	 * @param value
	 * @return
	 * @throws ClassNotFoundException
	 */
	protected Class<?> resolveTargetType(TypedStringValue value) throws ClassNotFoundException {
		if (value.hasTargetType()) {
			return value.getTargetType();
		}
		return value.resolveTargetType(beanFactory.getBeanClassLoader());
	}
	
	/**
	 * Resolve an inner bean definition.
	 * @param argName
	 * @param innerBeanName
	 * @param innerBeanDefinition
	 * @return
	 */
	private Object resolveInnerBean(Object argName, String innerBeanName, BeanDefinition innerBeanDefinition) {
		RootBeanDefinition mbd = null;
		try {
			mbd = beanFactory.getMergedBeanDefinition(innerBeanName, innerBeanDefinition, beanDefinition);
			String actualInnerBeanName = innerBeanName;
			if (mbd.isSingleton()) {
				actualInnerBeanName = adaptInnerBeanName(innerBeanName);
			}
			String[] dependsOn = mbd.getDependsOn();
			if (dependsOn != null) {
				for (String bean : dependsOn) {
					beanFactory.getBean(bean);
					beanFactory.registerDependentBean(bean, actualInnerBeanName);
				}
			}
			Object innerBean = beanFactory.createBean(actualInnerBeanName, mbd, null);
			beanFactory.registerContainedBean(actualInnerBeanName, beanName);
			if (innerBean instanceof FactoryBean) {
				boolean synthetic = (mbd != null && mbd.isSynthetic());
				return beanFactory.getObjectFromFactoryBean((FactoryBean<?>) innerBean, actualInnerBeanName, !synthetic);
			}
			else {
				return innerBean;
			}
		} 
		catch (BeansException ex) {
			throw new BeanCreationException(beanDefinition.getResourceDescription(), 
				beanName, String.format("Cannot create inner bean '%s' %swhile setting %s", 
					innerBeanName, (mbd != null && mbd.getBeanClassName() != null ? "of type [" + mbd.getBeanClassName() + "] " : ""), argName), ex);
		}
	}
	
	/**
	 * Holder class used for delayed toString building.
	 */
	private static class KeyedArgName {
		
		private final Object argName;
		
		private final Object key;
		
		public KeyedArgName(Object argName, Object key) {
			this.argName = argName;
			this.key = key;
		}

		@Override
		public String toString() {
			return argName + " with key " + BeanWrapper.PROPERTY_KEY_PREFIX + key + BeanWrapper.PROPERTY_KEY_PREFIX;
		}
		
	}
}
