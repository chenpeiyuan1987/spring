package org.yuan.study.spring.core;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Test;
import org.yuan.study.spring.core.io.Resource;

public class GenericCollectionTypeResolverTest {

	@Test
	public void testGetMapValueReturnType() throws Exception {
		Method method = null;
		Type type = null;
		
		method = Foo.class.getDeclaredMethod("a");
		type = GenericCollectionTypeResolver.getMapValueReturnType(method);
		assertEquals(Integer.class, type);
		
		method = Foo.class.getDeclaredMethod("b");
		type = GenericCollectionTypeResolver.getMapValueReturnType(method);
		assertNull(type);
		
		method = Foo.class.getDeclaredMethod("b2");
		type = GenericCollectionTypeResolver.getMapValueReturnType(method);
		assertEquals(Set.class, type);
		
		method = Foo.class.getDeclaredMethod("b3");
		type = GenericCollectionTypeResolver.getMapValueReturnType(method);
		assertEquals(Set.class, type);
		
		method = Foo.class.getDeclaredMethod("c");
		type = GenericCollectionTypeResolver.getMapValueReturnType(method);
		assertNull(type);
		
		method = Foo.class.getDeclaredMethod("d");
		type = GenericCollectionTypeResolver.getMapValueReturnType(method);
		assertEquals(Integer.class, type);
		
		method = Foo.class.getDeclaredMethod("d2");
		type = GenericCollectionTypeResolver.getMapValueReturnType(method);
		assertEquals(Integer.class, type);
		
		method = Foo.class.getDeclaredMethod("d3");
		type = GenericCollectionTypeResolver.getMapValueReturnType(method);
		assertEquals(Integer.class, type);
		
		method = Foo.class.getDeclaredMethod("e");
		type = GenericCollectionTypeResolver.getMapValueReturnType(method);
		assertEquals(Integer.class, type);
		
		method = Foo.class.getDeclaredMethod("e2");
		type = GenericCollectionTypeResolver.getMapValueReturnType(method);
		assertEquals(Integer.class, type);
		
		method = Foo.class.getDeclaredMethod("e3");
		type = GenericCollectionTypeResolver.getMapValueReturnType(method);
		assertEquals(Integer.class, type);
	}
	
	@Test
	public void testProgrammaticListIntrospection() throws Exception {
		Method setter = GenericBean.class.getMethod("setResourceList", List.class);
		assertEquals(Resource.class, GenericCollectionTypeResolver.getCollectionParameterType(new MethodParameter(setter, 0)));
		
		Method getter = GenericBean.class.getMethod("getResourceList");
		assertEquals(Resource.class, GenericCollectionTypeResolver.getCollectionReturnType(getter));
	}
	
	@Test
	public void testClassResolution() {
		assertEquals(String.class, GenericCollectionTypeResolver.getCollectionType(CustomSet.class));
		assertEquals(String.class, GenericCollectionTypeResolver.getMapKeyType(CustomMap.class));
		assertEquals(Integer.class, GenericCollectionTypeResolver.getMapValueType(CustomMap.class));
	}
	//--------------------------------------------------------------
	// private class for test
	//--------------------------------------------------------------
	
	private abstract class CustomSet<T> extends AbstractSet<String> {}
	
	private abstract class CustomMap<T> extends AbstractMap<String, Integer> {}
	
	private abstract class OtherCustomMap<T> implements Map<String, Integer> {}
	
	@SuppressWarnings({"unused", "rawtypes"})
	private interface Foo {
		
		Map<String, Integer> a();
		
		Map<?, ?> b();
		
		Map<?, ? extends Set> b2();
		
		Map<?, ? super Set> b3();
		
		Map c();
		
		CustomMap<Date> d();
		
		CustomMap<?> d2();
		
		CustomMap d3();
		
		OtherCustomMap<Date> e();
		
		OtherCustomMap<?> e2();
		
		OtherCustomMap e3();
	}
	
	private class GenericBean {
		private List<Resource> resourceList;

		public List<Resource> getResourceList() {
			return resourceList;
		}

		public void setResourceList(List<Resource> resourceList) {
			this.resourceList = resourceList;
		}
		
	}
}
