package org.yuan.study.spring.beans.factory;

public interface BeanClassLoaderAware {

	/**
	 * Callback that supplies the bean 'ClassLoader' to a bean instance.
	 * @param classLoader
	 */
	void setBeanClassLoader(ClassLoader classLoader);
}
