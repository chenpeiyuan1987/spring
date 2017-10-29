package test.reflect.demo;

import java.awt.List;
import java.lang.annotation.ElementType;
import java.lang.reflect.Modifier;
import java.util.AbstractMap;
import java.util.HashMap;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;

public class Main {

	@Test
	public void test() {
		System.out.println(Map.class);
		System.out.println(Map.class.getClass());
		Assert.assertTrue(Map.class.getClass() == List.class.getClass());
	}
	
	//@Test
	public void testClassInfo() {
		ClassInfo.setFullName(false);
		System.out.println(ClassInfo.getClassInfo(Map.class));
		
		System.out.println(ClassInfo.getClassInfo(HashMap.class));
		System.out.println(ClassInfo.getClassInfo(AbstractMap.class));
		
		System.out.println(ClassInfo.getClassInfo(ElementType.class));
		
		System.out.println(ClassInfo.getClassInfo(ITested.class));
	}
	
	//@Test
	public void testModifiers() {
		int[] modifiers = {
				// 修饰符
				Modifier.PUBLIC,		//1
				Modifier.PRIVATE,		//10
				Modifier.PROTECTED,		//100
				Modifier.STATIC,		//1000
				Modifier.FINAL,			//10000
				Modifier.SYNCHRONIZED,	//100000
				Modifier.VOLATILE,		//1000000
				Modifier.TRANSIENT,		//10000000
				Modifier.NATIVE,		//100000000
				Modifier.INTERFACE,		//1000000000
				Modifier.ABSTRACT,		//10000000000
				Modifier.STRICT,		//100000000000
				// 类修饰符
				Modifier.classModifiers(),		//110000011111 	- STRICT,ABSTRACT,STATIC,FINAL,PROTECTED,PUBLIC,PRIVATE
				// 构造函数修饰符
				Modifier.constructorModifiers(),//111 			- PROTECTED,PUBLIC,PRIVATE
				// 字段修饰符
				Modifier.fieldModifiers(),		//11011111 		- TRANSIENT,VOLATILE,STATIC,FINAL,PROTECTED,PUBLIC,PRIVATE
				// 接口修饰符
				Modifier.interfaceModifiers(),	//110000001111 	- STRICT,ABSTRACT,STATIC,PROTECTED,PUBLIC,PRIVATE
				// 方法修饰符
				Modifier.methodModifiers(),		//110100111111 	- STRICT,ABSTRACT,NATIVE,SYNCHRONIZED,STRICT,ABSTRACT,PROTECTED,PUBLIC,PRIVATE
				// 参数修饰符
				Modifier.parameterModifiers(),	//10000 		- FINAL
			};
			
			for (int i : modifiers) {
				System.out.println(Integer.toBinaryString(i));
			}
	}
}

