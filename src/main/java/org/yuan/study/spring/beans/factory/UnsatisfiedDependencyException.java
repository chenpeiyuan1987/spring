package org.yuan.study.spring.beans.factory;

import org.springframework.util.ClassUtils;

public class UnsatisfiedDependencyException extends BeanCreationException {
	private static final long serialVersionUID = 1L;

	/**
	 * Create a new UnsatisfiedDependencyException.
	 * @param resourceDescription
	 * @param beanName
	 * @param propertyName
	 * @param message
	 */
	public UnsatisfiedDependencyException(String resourceDescription, String beanName, String propertyName, String message) {
		super(resourceDescription, beanName, 
			String.format("Unsatisfied dependency expressed through bean property '%s'%s", 
				propertyName, (message != null ? ": " + message : "")));
	}
	
	/**
	 * Create a new UnsatisfiedDependencyException.
	 * @param resourceDescription
	 * @param beanName
	 * @param ctorArgIndex
	 * @param ctorArgType
	 * @param message
	 */
	public UnsatisfiedDependencyException(String resourceDescription, String beanName, int ctorArgIndex, Class<?> ctorArgType, String message) {
		super(resourceDescription, beanName, 
			String.format("Unsatisfied dependency expressed through constructor argument with index %s of type [%s]%s", 
				ctorArgIndex, ClassUtils.getQualifiedName(ctorArgType), (message != null ? ": " + message : "")));
	}

}
