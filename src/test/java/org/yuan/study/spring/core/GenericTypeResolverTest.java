package org.yuan.study.spring.core;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.Collection;

import org.junit.Test;

public class GenericTypeResolverTest {

	@Test
	public void testSimpleInterfaceType() {
		assertEquals(String.class, GenericTypeResolver.resolveTypeArgument(MySimpleInterfaceType.class, MyInterfaceType.class));
	}
	
	@Test
	public void testSimpleCollectionInterfaceType() {
		assertEquals(Collection.class, GenericTypeResolver.resolveTypeArgument(MyCollectionInterfaceType.class, MyInterfaceType.class));
	}
	
	@Test
	public void testSimpleSuperclassType() {
		assertEquals(String.class, GenericTypeResolver.resolveTypeArgument(MySimpleSuperclassType.class, MySuperclassType.class));
	}
	
	@Test
	public void testSimpleCollectionSuperclassType() {
		assertEquals(Collection.class, GenericTypeResolver.resolveTypeArgument(MyCollectionSuperclassType.class, MySuperclassType.class));
	}
	
	@Test
	public void testNullIfNotResolvable() {
		assertNull(GenericTypeResolver.resolveTypeArgument(new GenericClass<String>().getClass(), GenericClass.class));
	}
	
	//---------------------------------------------------------
	// class for test
	//---------------------------------------------------------
	
	public interface MyInterfaceType<T> {
		
	}
	
	public class MySimpleInterfaceType implements MyInterfaceType<String> {
		
	}
	
	public class MyCollectionInterfaceType implements MyInterfaceType<Collection<String>> {
		
	}
	
	public abstract class MySuperclassType<T> {
		
	}
	
	public class MySimpleSuperclassType extends MySuperclassType<String> {
		
	}
	
	public class MyCollectionSuperclassType extends MySuperclassType<Collection<String>> {
		
	}
	
	static class GenericClass<T> {
		
	}
}
