package org.yuan.study.spring;


import org.junit.Assert;
import org.junit.Test;

public class ClassTest {

	@Test
	public void testIsInstance() {
		Class<?> clazz = A1.class;
		
		Assert.assertTrue(clazz.isInstance(new A1()));
		Assert.assertTrue(clazz.isInstance(new B1()));
		Assert.assertTrue(clazz.isInstance(new C1()));
		Assert.assertFalse(clazz.isInstance(new A2()));
		Assert.assertFalse(clazz.isInstance(null));
	}
	
	@Test
	public void testIsAssignableFrom() {
		Class<?> clazz = A1.class;
		
		Assert.assertTrue(clazz.isAssignableFrom(A1.class));
		Assert.assertTrue(clazz.isAssignableFrom(B1.class));
		Assert.assertTrue(clazz.isAssignableFrom(C1.class));
		Assert.assertFalse(clazz.isAssignableFrom(A2.class));
		//Assert.assertFalse(clazz.isAssignableFrom(null));
	}
	
	@Test
	public void testEquals() {
		Assert.assertTrue(A1.class.equals(A1.class));
		Assert.assertFalse(A1.class.equals(B1.class));
		Assert.assertFalse(A1.class.equals(C1.class));
		Assert.assertFalse(A1.class.equals(A2.class));
	}
	
	@Test
	public void testInstanceof() {
		Object obj = null;
		
		obj = new A1();
		Assert.assertTrue(obj instanceof A1);
		obj = new B1();
		Assert.assertTrue(obj instanceof A1);
		obj = new C1();
		Assert.assertTrue(obj instanceof A1);
		obj = new A2();
		Assert.assertFalse(obj instanceof A1);
	}
	
	static class A1 {}
	static class A2 {}
	static class B1 extends A1 {}
	static class C1 extends B1 {}
}

