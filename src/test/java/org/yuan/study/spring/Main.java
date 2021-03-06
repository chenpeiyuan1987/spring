package org.yuan.study.spring;

import org.junit.Assert;
import org.junit.Test;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.xml.XmlBeanFactory;
import org.yuan.study.spring.core.io.ClassPathResource;

public class Main {

	@Test
	public void testGetBean() {
		BeanFactory factory = new XmlBeanFactory(new ClassPathResource("beans.xml", Main.class));
		
		Object hello = factory.getBean("hello");
		hello = factory.getBean("hello");
		
		Assert.assertTrue(hello instanceof Hello);
	}
	
}
