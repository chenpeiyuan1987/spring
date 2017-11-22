package test.reflect;

import java.lang.reflect.GenericArrayType;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;

import test.reflect.Circle.Point;


public class Main {

	@Test
	public void testClass() {
		Class<?> clazz = Circle.class;
		
		Assert.assertEquals("Circle", clazz.getSimpleName());
		Assert.assertEquals("test.reflect.Circle", clazz.getName());
		Assert.assertEquals("test.reflect.Circle", clazz.getCanonicalName());
		Assert.assertEquals("test.reflect.Circle", clazz.getTypeName());
		Assert.assertEquals(Object.class, clazz.getSuperclass());
		Assert.assertEquals(Object.class, clazz.getGenericSuperclass());
		Assert.assertEquals(0, clazz.getGenericInterfaces().length);
		Assert.assertTrue(Arrays.equals(new Object[] {Point.class}, clazz.getClasses()));
	}
	
	@Test
	public void testGenericClass() {
		Class<?> clazz = Packet.class;
		
		Assert.assertEquals("Packet", clazz.getSimpleName());
		Assert.assertEquals("test.reflect.Packet", clazz.getName());
		Assert.assertEquals("test.reflect.Packet", clazz.getCanonicalName());
		Assert.assertEquals("test.reflect.Packet", clazz.getTypeName());
		Assert.assertEquals(ArrayList.class, clazz.getSuperclass());
		Assert.assertTrue(ParameterizedType.class.isAssignableFrom(clazz.getGenericSuperclass().getClass()));
		Assert.assertEquals(0, clazz.getGenericInterfaces().length);
		Assert.assertEquals(0, clazz.getClasses().length);
		
		ParameterizedType paramType = (ParameterizedType)clazz.getGenericSuperclass();
		Assert.assertEquals(ArrayList.class, paramType.getRawType());
		Assert.assertTrue(Arrays.equals(new Object[] {Long.class}, paramType.getActualTypeArguments()));
	}
	
	@Test
	public void testGenericClass2() {
		Class<?> clazz = Plane.class;
		
		Assert.assertEquals("Plane", clazz.getSimpleName());
		Assert.assertEquals("test.reflect.Plane", clazz.getName());
		Assert.assertEquals("test.reflect.Plane", clazz.getCanonicalName());
		Assert.assertEquals("test.reflect.Plane", clazz.getTypeName());
		Assert.assertEquals(ArrayList.class, clazz.getSuperclass());
		Assert.assertTrue(ParameterizedType.class.isAssignableFrom(clazz.getGenericSuperclass().getClass()));
		Assert.assertEquals(0, clazz.getGenericInterfaces().length);
		Assert.assertEquals(0, clazz.getClasses().length);
		
		ParameterizedType paramType = (ParameterizedType)clazz.getGenericSuperclass();
		Assert.assertEquals(ArrayList.class, paramType.getRawType());
		Assert.assertTrue(ParameterizedType.class.isInstance(paramType.getActualTypeArguments()[0]));
		
		paramType = (ParameterizedType)paramType.getActualTypeArguments()[0];
		System.out.println(paramType.getOwnerType());
	}
	
	@Test
	public void testTypeVariableWithInterface() {
		Class<?> ifc = MyList.class;
		
		Assert.assertNull(ifc.getSuperclass());
		Assert.assertNull(ifc.getGenericSuperclass());
		Assert.assertArrayEquals(new Class<?>[]{List.class}, ifc.getInterfaces());
		
		Type type = ifc.getGenericInterfaces()[0];
		ParameterizedType pt = (ParameterizedType) type;
		Assert.assertEquals(List.class, pt.getRawType());
		Assert.assertArrayEquals(new Type[]{String.class}, pt.getActualTypeArguments());
		
		TypeVariable<?> tv = (TypeVariable<?>)((Class) pt.getRawType()).getTypeParameters()[0];
		Assert.assertArrayEquals(new Type[]{Object.class}, tv.getBounds());
	}
	
	@Test
	public void testTypeVariableWithMyInterface() {
		TypeVariable<?>[] tvs = (TypeVariable<?>[])A.class.getTypeParameters();
		Assert.assertArrayEquals(new Type[]{I1.class}, tvs[0].getBounds());
		Assert.assertArrayEquals(new Type[]{I2.class}, tvs[1].getBounds());
	}
	
	
	@Test
	public void testWildcardType() throws Exception {
		ParameterizedType pt = (ParameterizedType) B.class.getDeclaredField("list1").getGenericType();
		WildcardType tw = (WildcardType) pt.getActualTypeArguments()[0];
		Assert.assertEquals(Number.class, tw.getLowerBounds()[0]);
		
		pt = (ParameterizedType) B.class.getDeclaredField("list2").getGenericType();
		tw = (WildcardType) pt.getActualTypeArguments()[0];
		Assert.assertEquals(Number.class, tw.getUpperBounds()[0]);
	}
	
	@Test
	public void testGenericArrayType() throws Exception {
		Class<?> clazz = B.class.getDeclaredField("array").getType();
		Assert.assertEquals(List.class, clazz.getComponentType());
		
		Type type = B.class.getDeclaredField("array").getGenericType();
		Assert.assertTrue(type instanceof GenericArrayType);
		GenericArrayType gat = (GenericArrayType) type;
		ParameterizedType pt = (ParameterizedType)gat.getGenericComponentType();
		Assert.assertEquals(List.class, pt.getRawType());
		Assert.assertEquals(String.class, pt.getActualTypeArguments()[0]);
	}
	
	//-----------------------------------------------------------------
	// Class for test
	//-----------------------------------------------------------------
	
	private interface MyList extends List<String> {}
	
	private interface I1 {}
	
	private interface I2 {}
	
	@Deprecated
	private interface A<U extends I1, S extends I2> {}
	
	private class B {
		private List<String>[] array; 
		private List<? super Number> list1;
		private List<? extends Number> list2;
	}
}
