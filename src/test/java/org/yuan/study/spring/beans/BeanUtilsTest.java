package org.yuan.study.spring.beans;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.propertyeditors.CustomDateEditor;
import org.yuan.study.spring.core.io.Resource;
import org.yuan.study.spring.core.io.ResourceEditor;

import test.beans.DerivedTestBean;
import test.beans.ITestBean;
import test.beans.TestBean;

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
	public void testGetPropertyDescriptors() throws Exception {
		PropertyDescriptor[] actual = Introspector.getBeanInfo(TestBean.class).getPropertyDescriptors();
		PropertyDescriptor[] descriptors = BeanUtils.getPropertyDescriptors(TestBean.class);
		assertNotNull("Descriptors should not be null", descriptors);
		assertEquals(actual.length, descriptors.length);
	}
	
	@Test
	public void testBeanPropertyIsArray () {
		PropertyDescriptor[] descriptors = BeanUtils.getPropertyDescriptors(ContainerBean.class);
		for (PropertyDescriptor descriptor : descriptors) {
			if ("containedBeans".equals(descriptor.getName())) {
				assertTrue(descriptor.getPropertyType().isArray());
				assertEquals(descriptor.getPropertyType().getComponentType(), ContainedBean.class);
			}
		}
	}
	
	@Test
	public void testFindEditorByConvention() {
		assertEquals(ResourceEditor.class, BeanUtils.findEditorByConvention(Resource.class).getClass());
	}
	
	@Test
	public void testCopyProperties() throws Exception {
		TestBean tb1 = new TestBean();
		tb1.setName("rod");
		tb1.setAge(32);
		tb1.setTouchy("touchy");
		
		TestBean tb2 = new TestBean();
		assertTrue(tb2.getName() == null);
		assertTrue(tb2.getAge() == 0);
		assertTrue(tb2.getTouchy() == null);
		
		BeanUtils.copyProperties(tb1, tb2);
		assertEquals(tb1.getName(), tb2.getName());
		assertEquals(tb1.getAge(), tb2.getAge());
		assertEquals(tb1.getTouchy(), tb2.getTouchy());
	}
	
	@Test
	public void testCopyPropertiesWithDifferentTypes1() throws Exception {
		DerivedTestBean tb1 = new DerivedTestBean();
		tb1.setName("rod");
		tb1.setAge(32);
		tb1.setTouchy("touchy");
		
		TestBean tb2 = new TestBean();
		assertTrue(tb2.getName() == null);
		assertTrue(tb2.getAge() == 0);
		assertTrue(tb2.getTouchy() == null);
		
		BeanUtils.copyProperties(tb1, tb2);
		assertEquals(tb1.getName(), tb2.getName());
		assertEquals(tb1.getAge(), tb2.getAge());
		assertEquals(tb1.getTouchy(), tb2.getTouchy());
	}
	
	@Test
	public void testCopyPropertiesWithDifferentTypes2() throws Exception {
		TestBean tb1 = new TestBean();
		tb1.setName("rod");
		tb1.setAge(32);
		tb1.setTouchy("touchy");
		
		DerivedTestBean tb2 = new DerivedTestBean();
		assertTrue(tb2.getName() == null);
		assertTrue(tb2.getAge() == 0);
		assertTrue(tb2.getTouchy() == null);
		
		BeanUtils.copyProperties(tb1, tb2);
		assertEquals(tb1.getName(), tb2.getName());
		assertEquals(tb1.getAge(), tb2.getAge());
		assertEquals(tb1.getTouchy(), tb2.getTouchy());
	}
	
	@Test
	public void testCopyPropertiesWithEditable() throws Exception {
		TestBean tb1 = new TestBean();
		tb1.setName(null);
		tb1.setAge(32);
		tb1.setTouchy("bla");
		
		TestBean tb2 = new TestBean();
		tb2.setName("rod");
		tb2.setAge(0);
		tb2.setTouchy(null);
		
		BeanUtils.copyProperties(tb1, tb2, ITestBean.class);
		assertTrue(tb2.getName() == null);
		assertTrue(tb2.getAge() == 32);
		assertTrue(tb2.getTouchy() == null);
	}
	
	@Test
	public void testCopyPropertiesWithIgnore() throws Exception {
		TestBean tb1 = new TestBean();
		tb1.setName(null);
		tb1.setAge(32);
		tb1.setTouchy("bla");
		
		TestBean tb2 = new TestBean();
		tb2.setName("rod");
		tb2.setAge(0);
		tb2.setTouchy(null);
		
		BeanUtils.copyProperties(tb1, tb2, new String[] {"spouse", "touchy", "age"});
		assertTrue(tb2.getName() == null);
		assertTrue(tb2.getAge() == 0);
		assertTrue(tb2.getTouchy() == null);
	}
	
	@Test
	public void testCopyPropertiesWithIgnoreNonExistingProperty() {
		NameAndSpecialProperty source = new NameAndSpecialProperty();
		source.setName("name");
		TestBean target = new TestBean();
		BeanUtils.copyProperties(source, target, new String[]{"specialProperty"});
		assertEquals(target.getName(), "name");
	}
	
	@Test
	public void testResolveSimpleSignature() throws Exception {
		Method desiredMethod = MethodSignatureBean.class.getMethod("doSomething");
		assertSignatureEquals(desiredMethod, "doSomething");
		assertSignatureEquals(desiredMethod, "doSomething()");
	}
	
	@Test
	public void testResolveInvalidSignature() throws Exception {
		try {
			BeanUtils.resolveSignature("doSomething(", MethodSignatureBean.class);
			fail("Should not be able to parse with opening but no closing paren.");
		}
		catch (IllegalArgumentException ex) {}
		
		try {
			BeanUtils.resolveSignature("doSomething)", MethodSignatureBean.class);
			fail("Should not be able to parse with closing but not no opening paren.");
		} 
		catch (IllegalArgumentException  e) {}
	}
	
	@Test
	public void testResolveWithAndWithoutArgList() throws Exception {
		Method desiredMethod = MethodSignatureBean.class.getMethod("doSomethingElse", new Class[]{String.class, int.class});
		assertSignatureEquals(desiredMethod, "doSomethingElse");
		assertNull(BeanUtils.resolveSignature("doSomethingElse()", MethodSignatureBean.class));
	}
	
	@Test
	public void testResolveTypedSignature() throws Exception {
		Method desiredMethod = MethodSignatureBean.class.getMethod("doSomethingElse", new Class[]{String.class, int.class});
		assertSignatureEquals(desiredMethod, "doSomethingElse(java.lang.String, int)");
	}
	
	@Test
	public void testResolveOverloadedSignature() throws Exception {
		Method desiredMethod = MethodSignatureBean.class.getMethod("overloaded");
		assertSignatureEquals(desiredMethod, "overloaded()");

		desiredMethod = MethodSignatureBean.class.getMethod("overloaded", new Class[]{String.class});
		assertSignatureEquals(desiredMethod, "overloaded(java.lang.String)");
		
		desiredMethod = MethodSignatureBean.class.getMethod("overloaded", new Class[]{String.class, BeanFactory.class});
		assertSignatureEquals(desiredMethod, "overloaded(java.lang.String, org.yuan.study.spring.beans.factory.BeanFactory)");
	}
	
	@Test
	public void testResolveSignatureWithArray() throws Exception {
		Method desiredMethod = MethodSignatureBean.class.getMethod("doSomethingWithAnArray", new Class[]{String[].class});
		assertSignatureEquals(desiredMethod, "doSomethingWithAnArray(java.lang.String[])");
		
		desiredMethod = MethodSignatureBean.class.getMethod("doSomethingWithAMultiDimensionalArray", new Class[]{String[][].class});
		assertSignatureEquals(desiredMethod, "doSomethingWithAMultiDimensionalArray(java.lang.String[][])");
	}
	
	@Test
	public void testSPR6063() {
		PropertyDescriptor[] descrs = BeanUtils.getPropertyDescriptors(Bean.class);
		
		PropertyDescriptor keyDescr = BeanUtils.getPropertyDescriptor(Bean.class, "value");
		assertEquals(String.class, keyDescr.getPropertyType());
		for (PropertyDescriptor descr : descrs) {
			if (descr.getName().equals(keyDescr.getName())) {
				assertEquals(keyDescr.getPropertyType(), descr.getPropertyType());
			}
		}
	}
	
	private void assertSignatureEquals(Method desiredMethod, String signature) {
		assertEquals(desiredMethod, BeanUtils.resolveSignature(signature, MethodSignatureBean.class));
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
	
	private interface MapEntry<K,V> {
		
		K getKey();
		
		void setKey(V vallue);
		
		V getValue();
		
		void setValue(V value);
		
	}
	
	private static class Bean implements MapEntry<String,String> {
		private String key;
		private String value;
		
		@Override
		public void setKey(String key) {
			this.key = key;
		}
		
		@Override
		public void setValue(String value) {
			this.value = value;
		}
		
		@Override
		public String getKey() {
			return key;
		}
		
		@Override
		public String getValue() {
			return value;
		}
		
	}
}
