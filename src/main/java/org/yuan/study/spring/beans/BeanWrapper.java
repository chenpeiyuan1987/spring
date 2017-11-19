package org.yuan.study.spring.beans;

import java.beans.PropertyDescriptor;

public interface BeanWrapper extends ConfigurablePropertyAccessor {
	
	/**
	 * Return the bean instance wrapped by this object, if any.
	 * @return
	 */
	Object getWrappedInstance();
	
	/**
	 * Return the type of the wrapped JavaBean object.
	 * @return
	 */
	Class<?> getWrappedClass();
	
	/**
	 * Obtain the PropertyDescriptors for the wrapped object.
	 * @return
	 */
	PropertyDescriptor[] getPropertyDescriptors();
	
	/**
	 * Obtain the property descriptor or a specific property of the wrapped object.
	 * @param propertyName
	 * @return
	 */
	PropertyDescriptor getPropertyDescriptor(String propertyName);
	
	/**
	 * Set whether this BeanWrapper should attempt to "auto-grow" a nested path that contains a null value.
	 */
	void setAutoGrowNestedPaths(boolean autoGrowNestedPaths);
	
	/**
	 * Return whether "auto-growing" of nested paths has been activated.
	 */
	boolean isAutoGrowNestedPaths();
	
	/**
	 * Specify a limit for array and collection auto growing.
	 */
	void setAutoGrowCollectionLimit(int autoGrowCollectionLimit);
	
	/**
	 * Return the limit for array and collection auto growing.
	 */
	int getAutoGrowCollectionLimit();
}
