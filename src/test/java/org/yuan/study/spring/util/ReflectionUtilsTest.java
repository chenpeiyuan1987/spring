package org.yuan.study.spring.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.rmi.ConnectException;
import java.rmi.RemoteException;
import java.util.LinkedList;
import java.util.List;

import org.junit.Test;

import test.beans.TestBean;

public class ReflectionUtilsTest {

	@Test
	public void testFindField() {
		Field field = ReflectionUtils.findField(TestBeanSubclassWithField.class, "publ", String.class);
		assertNotNull(field);
		assertEquals("publ", field.getName());
		assertEquals(String.class, field.getType());
		assertTrue(Modifier.isPublic(field.getModifiers()));
		
		field = ReflectionUtils.findField(TestBeanSubclassWithField.class, "prot", String.class);
		assertNotNull(field);
		assertEquals("prot", field.getName());
		assertEquals(String.class, field.getType());
		assertTrue(Modifier.isProtected(field.getModifiers()));
		
		field = ReflectionUtils.findField(TestBeanSubclassWithField.class, "priv", String.class);
		assertNotNull(field);
		assertEquals("priv", field.getName());
		assertEquals(String.class, field.getType());
		assertTrue(Modifier.isPrivate(field.getModifiers()));
	}
	
	@Test
	public void testSetField() {
		TestBeanSubclassWithField testBean = new TestBeanSubclassWithField();
		Field field = ReflectionUtils.findField(TestBeanSubclassWithField.class, "name", String.class);
		
		ReflectionUtils.makeAccessible(field);
		
		ReflectionUtils.setField(field, testBean, "FooBar");
		assertEquals("FooBar", testBean.getName());
		
		ReflectionUtils.setField(field, testBean, null);
		assertNull(testBean.getName());
	}
	
	@Test(expected=IllegalStateException.class)
	public void testSetFieldIllegal() {
		TestBeanSubclassWithField testBean = new TestBeanSubclassWithField();
		Field field = ReflectionUtils.findField(TestBeanSubclassWithField.class, "name", String.class);
		ReflectionUtils.setField(field, testBean, "FooBar");
	}
	
	@Test
	public void testInvokeMethod() throws Exception {
		String chen = "chen";
		
		TestBean bean = new TestBean();
		bean.setName(chen);
		
		Method getName = TestBean.class.getMethod("getName");
		Method setName = TestBean.class.getMethod("setName", new Class[]{String.class});
		
		Object name = ReflectionUtils.invokeMethod(getName, bean);
		assertEquals(chen, name);
		
		String yuan = "yuan";
		ReflectionUtils.invokeMethod(setName, bean, yuan);
		assertEquals(yuan, bean.getName());
	}
	
	@Test
	public void testDeclaresException() throws Exception {
		Method remoteExMethod = A.class.getDeclaredMethod("foo", Integer.class);
		assertTrue(ReflectionUtils.declaresException(remoteExMethod, RemoteException.class));
		assertTrue(ReflectionUtils.declaresException(remoteExMethod, ConnectException.class));
		assertFalse(ReflectionUtils.declaresException(remoteExMethod, NoSuchMethodException.class));
		assertFalse(ReflectionUtils.declaresException(remoteExMethod, Exception.class));
		
		Method illegalExMethod = B.class.getDeclaredMethod("bar", String.class);
		assertTrue(ReflectionUtils.declaresException(illegalExMethod, IllegalArgumentException.class));
		assertTrue(ReflectionUtils.declaresException(illegalExMethod, NumberFormatException.class));
		assertFalse(ReflectionUtils.declaresException(illegalExMethod, IllegalStateException.class));
		assertFalse(ReflectionUtils.declaresException(illegalExMethod, Exception.class));
	}
	
	@Test
	public void testShallowCopyFieldStateForInvalid() {
		TestBean src = new TestBean();
		String dst = new String();
		try {
			ReflectionUtils.shallowCopyFieldState(src, dst);
			fail();
		} catch (IllegalArgumentException e) {}
		
		src = null;
		dst = new String();
		try {
			ReflectionUtils.shallowCopyFieldState(src, dst);
			fail();
		} catch (IllegalArgumentException e) {}
		
		src = new TestBean();
		dst = null;
		try {
			ReflectionUtils.shallowCopyFieldState(src, dst);
			fail();
		} catch (IllegalArgumentException e) {}
		
	}
	
	@Test
	public void testShallowCopyFieldStateForValid() {
		testValidCopy(new TestBean(), new TestBean());
		
		testValidCopy(new TestBeanSubclassWithField(), new TestBeanSubclassWithField());
		
		TestBean src = new TestBeanSubclassWithField();
		TestBean dst = new TestBeanSubclassWithField();
		((TestBeanSubclassWithField)src).priv = "11";
		testValidCopy(src, dst);
		assertEquals(((TestBeanSubclassWithField)src).priv, ((TestBeanSubclassWithField)src).priv);
		assertEquals(((TestBeanSubclassWithField)src).prot, ((TestBeanSubclassWithField)src).prot);
		
		src = new TestBean();
		dst = new TestBeanSubclassWithField();
		((TestBeanSubclassWithField)dst).priv = "11";
		testValidCopy(src, dst);
		assertEquals("11", ((TestBeanSubclassWithField)dst).priv);
	}
	
	private void testValidCopy(TestBean src, TestBean dst) {
		src.setName("chen");
		src.setAge(15);
		src.setSpouse(new TestBean());
		assertFalse(src.getAge() == dst.getAge());
		
		ReflectionUtils.shallowCopyFieldState(src, dst);
		assertEquals(src.getAge(), dst.getAge());
		assertEquals(src.getSpouse(), dst.getSpouse());
		assertEquals(src.getDoctor(), dst.getDoctor());
	}
	
	@Test
	public void doWithProtectedMethods() {
		ListSavingMethodCallback mc = new ListSavingMethodCallback();
		ReflectionUtils.doWithMethods(TestBean.class, mc, new ReflectionUtils.MethodFilter() {
			@Override
			public boolean matches(Method method) {
				return Modifier.isProtected(method.getModifiers());
			}
		});
		assertFalse(mc.getMethodNames().isEmpty());
		assertTrue(mc.getMethodNames().contains("clone"));
		assertTrue(mc.getMethodNames().contains("finalize"));
		assertFalse(mc.getMethodNames().contains("hashCode"));
		assertFalse(mc.getMethodNames().contains("absquatulate"));
	}
	
	@Test
	public void duplicatesFound() {
		ListSavingMethodCallback mc = new ListSavingMethodCallback();
		ReflectionUtils.doWithMethods(TestBeanSubclassWithField.class, mc);
		
		int count = 0;
		for (String name : mc.getMethodNames()) {
			if (name.endsWith("absquatulate")) {
				count++;
			}
		}
		assertEquals(2, count);
	}
	
	@Test
	public void findMethod() {
		assertNotNull(ReflectionUtils.findMethod(B.class, "bar", String.class));
		assertNotNull(ReflectionUtils.findMethod(B.class, "foo", Integer.class));
		assertNotNull(ReflectionUtils.findMethod(B.class, "getClass"));
	}
	
	//----------------------------------------------------------------
	// Private class for test
	//----------------------------------------------------------------
	private static class ListSavingMethodCallback implements ReflectionUtils.MethodCallback {

		private List<String> methodNames = new LinkedList<String>();
		
		private List<Method> methods = new LinkedList<Method>();
		
		@Override
		public void doWith(Method method) throws IllegalArgumentException, IllegalAccessException {
			methodNames.add(method.getName());
			methods.add(method);
		}

		public List<String> getMethodNames() {
			return methodNames;
		}

		public List<Method> getMethods() {
			return methods;
		}
		
	}
	
	private static class TestBeanSubclassWithField extends TestBean {
		public String publ = "";
		private String priv = "";
		protected String prot = "";
		
		private final String fina = "";

		@Override
		public void absquatulate() {
		}
	}
	
	private static class A {
		private void foo(Integer i) throws RemoteException {
			
		}
	}
	
	private static class B extends A {
		void bar(String s) throws IllegalArgumentException {
			
		}
	}
}
