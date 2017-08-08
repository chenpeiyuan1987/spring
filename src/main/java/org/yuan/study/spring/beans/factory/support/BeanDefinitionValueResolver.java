package org.yuan.study.spring.beans.factory.support;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.config.BeanDefinitionHolder;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.config.TypedStringValue;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.beans.factory.support.ManagedMap;
import org.springframework.beans.factory.support.ManagedSet;
import org.yuan.study.spring.beans.BeanWrapper;
import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;

public class BeanDefinitionValueResolver {
	
	protected static final Log logger = LogFactory.getLog(BeanDefinitionValueResolver.class);

	private final AbstractBeanFactory beanFactory;
	
	private final String beanName;
	
	private final BeanDefinition beanDefinition;
	
	/**
	 * Create a new BeanDefinitionValueResolver for the given BeanFactory and BeanDefinition.
	 * @param beanFactory
	 * @param beanName
	 * @param beanDefinition
	 */
	public BeanDefinitionValueResolver(AbstractBeanFactory beanFactory, String beanName, BeanDefinition beanDefinition) {
		this.beanFactory = beanFactory;
		this.beanDefinition = beanDefinition;
		this.beanName = beanName;
	}
	
	/**
	 * Given a PropertyValue, return a value, resolving any references to other beans in the factory if necessary.
	 * @param argName
	 * @param value
	 * @return
	 */
	public Object resolveValueIfNecessary(String argName, Object value) throws BeansException {
		if (value instanceof BeanDefinitionHolder) {
			BeanDefinitionHolder beanDefinitionHolder = (BeanDefinitionHolder) value;
		}
		if (value instanceof BeanDefinition) {
			BeanDefinition beanDefinition = (BeanDefinition) value;
		}
		if (value instanceof RuntimeBeanReference) {
			RuntimeBeanReference runtimeBeanReference = (RuntimeBeanReference) value;
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
		if (value instanceof TypedStringValue) {
			TypedStringValue typedStringValue = (TypedStringValue) value;
		}
		
		return value;
	}
	
	private Object resolveInnerBeanDefinition(String argName, String innerBeanName, BeanDefinition innerBd) throws BeansException {
		
	}
	
	private Object resolveReference(String argName, RuntimeBeanReference runtimeBeanReference) throws BeansException {
		
	}
	
	/**
	 * For each element in the managedList, resolve reference if necessary.
	 * @param argName
	 * @param list
	 * @return
	 * @throws BeansException
	 */
	private List<?> resolveManagedList(String argName, List<?> list) throws BeansException {
		List<Object> resolved = new ArrayList<Object>(list.size());
		for (int i=0; i<list.size(); i++) {
			resolved.add(
				resolveValueIfNecessary(
					String.format("%s with key %s", argName, BeanWrapper.PROPERTY_KEY_PREFIX + i + BeanWrapper.PROPERTY_KEY_SUFFIX), list.get(i)));
		}
		return resolved;
	}
	
	/**
	 * For each element in the managedList, resolve reference if necessary.
	 * @param argName
	 * @param set
	 * @return
	 * @throws BeansException
	 */
	private Set<?> resolveManagedSet(String argName, Set<?> set) throws BeansException {
		Set<Object> resolved = new LinkedHashSet<Object>(set.size());
		int i = 0;
		for (Object object : set) {
			resolved.add(resolveValueIfNecessary(
				String.format("%s with key %s", argName, BeanWrapper.PROPERTY_KEY_PREFIX + i + BeanWrapper.PROPERTY_KEY_SUFFIX), object));
			i++;
		}
		return resolved;
	}
	
	/**
	 * For each element in the managedMap, resolve reference if necessary.
	 * @param argName
	 * @param map
	 * @return
	 * @throws BeansException
	 */
	private Map<?,?> resolveManagedMap(String argName, Map<?,?> map) throws BeansException {
		Map<Object,Object> resolved = new LinkedHashMap<Object,Object>();
		
		for (Entry<?,?> entry : map.entrySet()) {
			Object resolvedKey = resolveValueIfNecessary(argName, entry.getKey());
			Object resolvedVal = resolveValueIfNecessary(
				String.format("%s with key %s", argName, BeanWrapper.PROPERTY_KEY_PREFIX + entry.getKey() + BeanWrapper.PROPERTY_KEY_SUFFIX), entry.getValue());
			resolved.put(resolvedKey, resolvedVal);
		}
		
		return resolved;
	}

}
