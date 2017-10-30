package test.reflect;

import java.util.Arrays;

import org.junit.Test;

public class ClassDemo {
	private Class<?> anon;
	
	public ClassDemo() {
		anon = new Object(){}.getClass();
	}
	
	@Test
	public void test() {
		class LocalClass {}
		
		Class<?> cls = ClassDemo.class;
		
		System.out.println(Arrays.toString(cls.getClasses()));
		System.out.println(Arrays.toString(cls.getDeclaredClasses()));
		System.out.println(cls.getDeclaringClass());
		System.out.println(PublicStaticClass.class.getDeclaringClass());
		System.out.println(PublicStaticClass.class.isMemberClass());
		System.out.println(LocalClass.class.isLocalClass());
		System.out.println(LocalClass.class.getEnclosingClass());
		System.out.println(LocalClass.class.getEnclosingMethod());
		System.out.println(LocalClass.class.getEnclosingConstructor());
		System.out.println(anon.isAnonymousClass());
		System.out.println(anon.getEnclosingClass());
		System.out.println(anon.getEnclosingMethod());
		System.out.println(anon.getEnclosingConstructor());
	}
	//-------------------------------------------
	//
	//-------------------------------------------
	
	public static class PublicStaticClass {
		public static class SubPublicStaticClass {}
	}
	
	class PackageClass {}
	public class PublicClass {}
	private class PrivateClass {}
	protected class ProtectedClass {}
}
