package org.yuan.study.spring.beans;

import static org.junit.Assert.fail;

import java.beans.PropertyDescriptor;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.propertyeditors.CustomDateEditor;

public final class BeanUtilsTest {

	@Test
	public void testInstantiateClass() {
		BeanUtils.instantiateClass(ArrayList.class);
		
		try {
			BeanUtils.instantiateClass(List.class);
			fail("Should have thrown FatalBeanException");
		}
		catch (FatalBeanException ex) {}
		
		try {
			BeanUtils.instantiateClass(CustomDateEditor.class);
			fail("Should have thrown FatalBeanException");
		}
		catch (FatalBeanException ex) {}
	}
	
	@Test
	public void testBeanPropertyIsArray () {
	}
	
	private static class NameAndSpecialProperty {
		
		private String name;
		
		private int specialProperty;

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}

		public int getSpecialProperty() {
			return specialProperty;
		}

		public void setSpecialProperty(int specialProperty) {
			this.specialProperty = specialProperty;
		}
		
	}
	
	private static class ContainerBean {
		
		private ContainedBean[] containedBeans;

		public ContainedBean[] getContainedBeans() {
			return containedBeans;
		}

		public void setContainedBeans(ContainedBean[] containedBeans) {
			this.containedBeans = containedBeans;
		}
		
	}
	
	private static class ContainedBean {
		
		private String name;

		public String getName() {
			return name;
		}

		public void setName(String name) {
			this.name = name;
		}
		
	}
	
	private static class MethodSignatureBean {
		
		public void doSomething() {}
		
		public void doSomethingElse(String s, int x) {}
		
		public void overloaded() {}
		
		public void overloaded(String s) {}
		
		public void overloaded(String s, BeanFactory beanFactory) {}
		
		public void doSomethingWithAnArray(String[] strings) {}
		
		public void doSomethingWithAMultiDimensionalArray(String[][] strings) {}
	}
}
