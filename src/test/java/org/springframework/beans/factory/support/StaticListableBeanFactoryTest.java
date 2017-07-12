package org.springframework.beans.factory.support;


import java.util.Arrays;
import java.util.HashSet;

import org.junit.Assert;
import org.junit.Test;
import org.yuan.study.spring.Hello;
import org.yuan.study.spring.HelloWorld;

public class StaticListableBeanFactoryTest {
	
	@Test
	public void test() {
		StaticListableBeanFactory factory = new StaticListableBeanFactory();
		factory.addBean("a", new HelloWorld());
		factory.addBean("b", new HelloWorld());
		factory.addBean("c", new HelloWorld());
		
		Assert.assertTrue(factory.containsBean("a"));
		Assert.assertFalse(factory.containsBean("d"));
		Assert.assertTrue(factory.containsBeanDefinition("a"));
		Assert.assertFalse(factory.containsBeanDefinition("d"));
		Assert.assertEquals(0, factory.getAliases("a").length);
		Assert.assertEquals(0, factory.getAliases("b").length);
		Assert.assertEquals(3, factory.getBeanDefinitionCount());
		Assert.assertArrayEquals(new String[]{"a", "b", "c"}, factory.getBeanDefinitionNames());
		Assert.assertArrayEquals(new String[]{"a", "b", "c"}, factory.getBeanDefinitionNames(Hello.class));
		Assert.assertEquals(HelloWorld.class, factory.getType("a"));
		Assert.assertTrue(factory.isSingleton("a"));
		//Assert.assertFalse(factory.isSingleton("d"));
		Assert.assertEquals(factory.getBeansOfType(Hello.class).keySet(), new HashSet(Arrays.asList("a", "b", "c")));
		
		Object bean = factory.getBean("a");
		Assert.assertTrue(bean instanceof Hello);
	}
	
}

