package org.yuan.study.spring.beans.factory.support;


import org.junit.Assert;
import org.junit.Test;
import org.yuan.study.spring.Hello;
import org.yuan.study.spring.HelloWorld;

public class StaticListableBeanFactoryTest {
	
	@Test
	public void testGetBean() {
		StaticListableBeanFactory factory = new StaticListableBeanFactory();
		factory.addBean("hello", new HelloWorld());
		
		Object bean = factory.getBean("hello");
		Assert.assertTrue(bean instanceof Hello);
	}
}

