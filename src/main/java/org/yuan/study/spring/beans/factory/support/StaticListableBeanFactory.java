package org.yuan.study.spring.beans.factory.support;

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.factory.BeanCreationException;
import org.yuan.study.spring.beans.factory.BeanFactoryUtils;
import org.yuan.study.spring.beans.factory.BeanIsNotAFactoryException;
import org.yuan.study.spring.beans.factory.BeanNotOfRequiredTypeException;
import org.yuan.study.spring.beans.factory.FactoryBean;
import org.yuan.study.spring.beans.factory.ListableBeanFactory;
import org.yuan.study.spring.beans.factory.NoSuchBeanDefinitionException;
import org.yuan.study.spring.beans.factory.SmartFactoryBean;
import org.yuan.study.spring.core.annotation.AnnotationUtils;
import org.yuan.study.spring.util.StringUtils;

public class StaticListableBeanFactory implements ListableBeanFactory {
	/** Map from bean name to bean instance */
	private final Map<String,Object> beans = new HashMap<String,Object>();
	
	/**
	 * add a new singleton bean.
	 * @param name
	 * @param bean
	 */
	public void addBean(String name, Object bean) {
		this.beans.put(name, bean);
	}
	
	
	//-------------------------------------------------------------
	// Implementation of BeanFactory interface
	//-------------------------------------------------------------
	
	@Override
	public Object getBean(String name) throws BeansException {
		String beanName = BeanFactoryUtils.transformedBeanName(name);
		Object bean = this.beans.get(beanName);
		
		if(bean == null) {
			String definedBeans = StringUtils.collectionToCommaDelimitedString(this.beans.keySet());
			throw new NoSuchBeanDefinitionException(
				beanName, String.format("Defined beans are [%s]", definedBeans));
		}
		if(BeanFactoryUtils.isFactoryDereference(name) && !(bean instanceof FactoryBean)) {
			throw new BeanIsNotAFactoryException(beanName, bean.getClass());
		}
		if(bean instanceof FactoryBean && !BeanFactoryUtils.isFactoryDereference(name)) {
			try {
				return ((FactoryBean<?>) bean).getObject();
			}
			catch(Exception ex) {
				throw new BeanCreationException(
					beanName, "FactoryBean threw exception on object creation", ex);
			}
		}
		
		return bean;
	}

	@Override
	public <T> T getBean(String name, Class<T> requiredType) throws BeansException {
		Object bean = getBean(name);
		if (requiredType != null && !requiredType.isAssignableFrom(bean.getClass())) {
			throw new BeanNotOfRequiredTypeException(name, requiredType, bean.getClass());
		}
		return (T) bean;
	}

	@Override
	public <T> T getBean(Class<T> requiredType) throws BeansException {
		String[] beanNames = getBeanNamesForType(requiredType);
		if (beanNames.length == 1) {
			return getBean(beanNames[0], requiredType);
		}
		
		throw new NoSuchBeanDefinitionException(
			requiredType, "expected single bean but found " + beanNames.length);
	}

	@Override
	public Object getBean(String name, Object... args) throws BeansException {
		if (args != null) {
			throw new UnsupportedOperationException(
				"StaticListableBeanFactory does not support explicit bean creation arguments");
		}
		return getBean(name);
	}

	@Override
	public boolean isPrototype(String name) throws NoSuchBeanDefinitionException {
		Object bean = getBean(name);
		
		if (bean instanceof SmartFactoryBean) {
			return ((SmartFactoryBean<?>) bean).isPrototype();
		}
		if (bean instanceof FactoryBean) {
			return !((FactoryBean<?>) bean).isSingleton();
		}
		return false;
	}

	@Override
	public boolean isTypeMatch(String name, Class<?> targetType) throws NoSuchBeanDefinitionException {
		Class<?> type = getType(name);
		
		if (targetType == null) {
			return true;
		}
		if (type != null && targetType.isAssignableFrom(type)) {
			return true;
		}
		return false;
	}

	@Override
	public Class<?> getType(String name) {
		String beanName = BeanFactoryUtils.transformedBeanName(name);
		Object bean = this.beans.get(beanName);
		
		if(bean == null) {
			String definedBeans = StringUtils.collectionToCommaDelimitedString(this.beans.keySet());
			throw new NoSuchBeanDefinitionException(
				beanName, String.format("Defined beans are [%s]", definedBeans));
		}
		if(bean instanceof FactoryBean && !BeanFactoryUtils.isFactoryDereference(name)) {
			return ((FactoryBean<?>) bean).getObjectType();
		}
		return bean.getClass();
	}

	@Override
	public boolean isSingleton(String name) {
		Object bean = getBean(name);
		
		if(bean instanceof FactoryBean) {
			return ((FactoryBean<?>) bean).isSingleton();
		}
		return false;
	}

	@Override
	public boolean containsBean(String name) {
		return this.beans.containsKey(name);
	}

	@Override
	public String[] getAliases(String name) {
		return new String[0];
	}
	
	//-------------------------------------------------------------
	// Implementation of ListableBeanFactory interface
	//-------------------------------------------------------------

	@Override
	public boolean containsBeanDefinition(String name) {
		return this.beans.containsKey(name);
	}

	@Override
	public int getBeanDefinitionCount() {
		return this.beans.size();
	}

	@Override
	public String[] getBeanDefinitionNames() {
		return StringUtils.toStringArray(this.beans.keySet());
	}

	@Override
	public String[] getBeanNamesForType(Class<?> type) {
		return getBeanNamesForType(type, true, true);
	}

	@Override
	public String[] getBeanNamesForType(Class<?> type, boolean includeNonSingletons, boolean includeFactoryBeans) {
		boolean isFactoryType = (type != null && FactoryBean.class.isAssignableFrom(type));
		List<String> result = new ArrayList<String>();
		
		for(Entry<String,Object> entry : this.beans.entrySet()) {
			if(entry.getValue() instanceof FactoryBean && !isFactoryType) {
				if(includeFactoryBeans) {
					Class<?> objectType = ((FactoryBean<?>) entry.getValue()).getObjectType();
					if(objectType != null && (type == null || type.isAssignableFrom(objectType))) {
						result.add(entry.getKey());
					}
				}
			}
			else {
				if(type == null || type.isInstance(entry.getValue())) {
					result.add(entry.getKey());
				}
			}
		}
		return StringUtils.toStringArray(result);
	}

	@Override
	public <T> Map<String, T> getBeansOfType(Class<T> type) throws BeansException {
		return getBeansOfType(type, true, true);
	}

	@Override
	public <T> Map<String, T> getBeansOfType(Class<T> type, boolean includeNonSingletons, boolean includeFactoryBeans) throws BeansException {
		boolean isFactoryType = (type != null && FactoryBean.class.isAssignableFrom(type));
		Map<String, T> result = new HashMap<String, T>();
		
		for(Entry<String, Object> entry : beans.entrySet()) {
			if(entry.getValue() instanceof FactoryBean && !isFactoryType) {
				if(includeFactoryBeans) {
					FactoryBean<?> factory = (FactoryBean<?>)entry.getValue();
					Class<?> objectType = factory.getObjectType();
					if((includeNonSingletons || factory.isSingleton()) 
						&& objectType != null && (type == null || type.isAssignableFrom(objectType))) {
						result.put(entry.getKey(), getBean(entry.getKey(), type));
					}
				}
			}
			else {
				if(type == null || type.isInstance(entry.getValue())) {
					String beanName = entry.getKey();
					if(isFactoryType) {
						beanName = FACTORY_BEAN_PREFIX + beanName;
					}
					result.put(beanName, (T)entry.getValue());
				}
			}
		}
		
		return result;
	}

	@Override
	public Map<String, Object> getBeansWithAnnotation(Class<? extends Annotation> annotationType) throws BeansException {
		Map<String, Object> results = new LinkedHashMap<String, Object>();
		for (String beanName : this.beans.keySet()) {
			if (findAnnotationOnBean(beanName, annotationType) != null) {
				results.put(beanName, getBean(beanName));
			}
		}
		return results;
	}

	@Override
	public <T extends Annotation> T findAnnotationOnBean(String beanName, Class<T> annotationType) {
		return AnnotationUtils.findAnnotation(getType(beanName), annotationType);
	}
	
}
