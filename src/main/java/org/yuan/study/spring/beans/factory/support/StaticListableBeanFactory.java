package org.yuan.study.spring.beans.factory.support;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.xml.ws.spi.Invoker;

import org.springframework.util.StringUtils;
import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.factory.BeanCreationException;
import org.yuan.study.spring.beans.factory.BeanFactoryUtils;
import org.yuan.study.spring.beans.factory.BeanIsNotAFactoryException;
import org.yuan.study.spring.beans.factory.BeanNotOfRequiredTypeException;
import org.yuan.study.spring.beans.factory.FactoryBean;
import org.yuan.study.spring.beans.factory.ListableBeanFactory;
import org.yuan.study.spring.beans.factory.NoSuchBeanDefinitionException;

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
			throw new NoSuchBeanDefinitionException(beanName, 
				String.format("Defined beans are [%s]", StringUtils.collectionToCommaDelimitedString(this.beans.keySet())));
		}
		
		if(BeanFactoryUtils.isFactoryDereference(name) && !(bean instanceof FactoryBean)) {
			throw new BeanIsNotAFactoryException(beanName, bean.getClass());
		}
		
		if(bean instanceof FactoryBean && !BeanFactoryUtils.isFactoryDereference(name)) {
			try {
				return ((FactoryBean) bean).getObject();
			}
			catch(Exception ex) {
				throw new BeanCreationException(beanName, "FactoryBean threw exception on object creation", ex);
			}
		}
		
		return this.beans.get(name);
	}

	@Override
	public Object getBean(String name, Class<?> requiredType) throws BeansException {
		Object bean = getBean(name);
		
		if(requiredType != null && !requiredType.isAssignableFrom(bean.getClass())) {
			throw new BeanNotOfRequiredTypeException(name, requiredType, bean.getClass());
		}
		
		return bean;
	}
	
	@Override
	public Class<?> getType(String name) {
		String beanName = BeanFactoryUtils.transformedBeanName(name);
		
		Object bean = this.beans.get(beanName);
		if(bean == null) {
			throw new NoSuchBeanDefinitionException(beanName, 
				String.format("Defined beans are [%s]", StringUtils.collectionToCommaDelimitedString(this.beans.keySet())));
		}
		
		if(bean instanceof FactoryBean && !BeanFactoryUtils.isFactoryDereference(name)) {
			return ((FactoryBean) bean).getObjectType();
		}
		
		return bean.getClass();
	}

	@Override
	public boolean isSingleton(String name) {
		Object bean = getBean(name);
		
		if(bean instanceof FactoryBean) {
			return ((FactoryBean) bean).isSingleton();
		}
		
		return true;
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
	public String[] getBeanDefinitionNames(Class<?> type) {
		List<String> result = new ArrayList<String>();
		
		for(Entry<String,Object> entry : beans.entrySet()) {
			if(type.isInstance(entry.getValue())) {
				result.add(entry.getKey());
			}
		}
		
		return StringUtils.toStringArray(result);
	}

	@Override
	public String[] getBeanNamesForType(Class<?> type) {
		return getBeanNamesForType(type, true, true);
	}

	@Override
	public String[] getBeanNamesForType(Class<?> type,
		boolean includePrototypes, boolean includeFactoryBeans) {
		boolean isFactoryType = (type != null && FactoryBean.class.isAssignableFrom(type));
		List<String> result = new ArrayList<String>();
		
		for(Entry<String,Object> entry : this.beans.entrySet()) {
			if(entry.getValue() instanceof FactoryBean && !isFactoryType) {
				if(includeFactoryBeans) {
					Class<?> objectType = ((FactoryBean) entry.getValue()).getObjectType();
					if(objectType != null && type.isAssignableFrom(objectType)) {
						result.add(entry.getKey());
					}
				}
				continue;
			}
			
			if(type.isInstance(entry.getValue())) {
				result.add(entry.getKey());
			}
		}
		
		return StringUtils.toStringArray(result);
	}

	@Override
	public Map<String, Object> getBeansOfType(Class<?> type) throws BeansException {
		return getBeansOfType(type, true, true);
	}

	@Override
	public Map<String, Object> getBeansOfType(Class<?> type,
		boolean includePrototypes, boolean includeFactoryBeans) throws BeansException {
		boolean isFactoryType = (type != null && FactoryBean.class.isAssignableFrom(type));
		Map<String,Object> result = new HashMap<String,Object>();
		
		for(Entry<String, Object> entry : beans.entrySet()) {
			if(entry.getValue() instanceof FactoryBean && !isFactoryType) {
				if(includeFactoryBeans) {
					FactoryBean factory = (FactoryBean)entry.getValue();
					Class<?> objectType = factory.getObjectType();
					if(objectType != null && type.isAssignableFrom(objectType)) {
						result.put(entry.getKey(), factory.getObjectType());
					}
				}
				continue;
			}
			
			if(type.isInstance(entry.getValue())) {
				String beanName = entry.getKey();
				if(isFactoryType) {
					beanName = FACTORY_BEAN_PREFIX + beanName;
				}
				result.put(beanName, entry.getValue());
			}
		}
		
		return result;
	}

}
