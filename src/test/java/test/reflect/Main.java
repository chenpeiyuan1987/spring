package test.reflect;

import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.Arrays;

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
}
