package org.yuan.study.spring.beans.factory.config;

import org.yuan.study.spring.beans.factory.ListableBeanFactory;

public interface ConfigurableListableBeanFactory extends ListableBeanFactory,
	ConfigurableBeanFactory, AutowireCapableBeanFactory {
	
}
