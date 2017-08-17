package org.yuan.study.spring.beans.factory.support;

import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.yuan.study.spring.beans.MutablePropertyValues;
import org.yuan.study.spring.beans.factory.BeanDefinitionStoreException;
import org.yuan.study.spring.beans.factory.config.BeanDefinitionHolder;
import org.yuan.study.spring.beans.factory.config.ConstructorArgumentValues;
import org.yuan.study.spring.util.ClassUtils;

public abstract class BeanDefinitionReaderUtils {
	
	/**
	 * 
	 */
	public static final String GENERATED_BEAN_NAME_SEPARATOR = "#";
	
	/**
	 * Register the given bean definition with the given bean factory.
	 * @param holder
	 * @param registry
	 */
	public static void registerBeanDefinition(BeanDefinitionHolder holder, BeanDefinitionRegistry registry) 
		throws BeanDefinitionStoreException {
		registry.registerBeanDefinition(holder.getBeanName(), holder.getBeanDefinition());
		if (holder.getAliases() != null) {
			for (String alias : holder.getAliases()) {
				registry.registerAlias(holder.getBeanName(), alias);
			}
		}
	}
	
	/**
	 * 
	 * @param beanDefinition
	 * @param beanFactory
	 * @param isInnerBean
	 * @return
	 * @throws BeanDefinitionStoreException
	 */
	public static String generateBeanName(AbstractBeanDefinition beanDefinition, BeanDefinitionRegistry beanFactory, boolean isInnerBean) 
		throws BeanDefinitionStoreException {
		String generatedId = beanDefinition.getBeanClassName();
		if (generatedId == null) {
			if (beanDefinition instanceof ChildBeanDefinition) {
				generatedId = ((ChildBeanDefinition) beanDefinition).getParentName() + "$child";
			} 
			else if (beanDefinition.getFactoryBeanName() != null) {
				generatedId = beanDefinition.getFactoryBeanName() + "$created";
			}
		}
		if (!StringUtils.hasText(generatedId)) {
			throw new BeanDefinitionStoreException(beanDefinition.getResourceDescription(), "", 
				"Unnamed bean definition specifies neither 'class' nor 'parent' nor 'factory-bean' - can't generate bean name");
		}
		
		String id = generatedId;
		if (isInnerBean) {
			id = generatedId + GENERATED_BEAN_NAME_SEPARATOR+ ObjectUtils.getIdentityHexString(beanDefinition);
		}
		else {
			int counter = 0;
			while (beanFactory.containsBeanDefinition(id)) {
				counter++;
				id = generatedId + GENERATED_BEAN_NAME_SEPARATOR + counter;
			}
		}
		
		return id;
	}
	
	/**
	 * 
	 * @param className
	 * @param parent
	 * @param cargs
	 * @param pvs
	 * @param classLoader
	 * @return
	 * @throws ClassNotFoundException
	 */
	public static AbstractBeanDefinition createBeanDefinition(String className, String parent, ConstructorArgumentValues cargs, 
		MutablePropertyValues pvs, ClassLoader classLoader) throws ClassNotFoundException {
		Class<?> beanClass = null;
		if (className != null && classLoader != null) {
			beanClass = ClassUtils.forName(className, classLoader);
		}
		
		if (parent == null) {
			if (beanClass != null) {
				return new RootBeanDefinition(beanClass, cargs, pvs);
			} 
			else {
				return new RootBeanDefinition(className, cargs, pvs);
			}
		} 
		else {
			if (beanClass != null) {
				return new ChildBeanDefinition(parent, beanClass, cargs, pvs);
			} 
			else {
				return new ChildBeanDefinition(parent, className, cargs, pvs);
			}
		}
	}
	
}
