package org.yuan.study.spring.beans.factory;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.util.Assert;

public abstract class BeanFactoryUtils {
	
	/**
	 * Return the bean name, 
	 * stripping out the factory dereference prefix if necessary.
	 * @param name
	 * @return
	 */
	public static String transformedBeanName(String name) {
		Assert.notNull(name, "Name must not be null");
		
		if(name.startsWith(BeanFactory.FACTORY_BEAN_PREFIX)) {
			name = name.substring(BeanFactory.FACTORY_BEAN_PREFIX.length());
		}
		
		return name;
	}
	
	/**
	 * Return whether the given name is a factory dereference
	 * @param name
	 * @return
	 */
	public static boolean isFactoryDereference(String name) {
		return name.startsWith(BeanFactory.FACTORY_BEAN_PREFIX);
	}
	
	/**
	 * Return all beans of the given type of subtypes, also picking up beans defined in 
	 * ancestor bean factories if the current bean factory is a HierarchicalBeanFactory.
	 * The returned Map will only contain beans of this type.
	 * @param lbf
	 * @param type
	 * @return
	 * @throws BeansException
	 */
	public static Map<String,Object> beansOfTypeIncludingAncestors(ListableBeanFactory lbf, Class<?> type) throws BeansException {
		Map<String,Object> result = new HashMap<String,Object>();
		result.putAll(lbf.getBeansOfType(type));
		if (lbf instanceof HierarchicalBeanFactory) {
			HierarchicalBeanFactory hbf = (HierarchicalBeanFactory) lbf;
			if (hbf.getParentBeanFactory() instanceof ListableBeanFactory) {
				Map<String,Object> parentResult = beansOfTypeIncludingAncestors((ListableBeanFactory) hbf.getParentBeanFactory(), type);
				for (Entry<String,Object> entry : parentResult.entrySet()) {
					if (!result.containsKey(entry.getKey())) {
						result.put(entry.getKey(), entry.getValue());
					}
				}
			}
		}
		
		return result;
	}
}
