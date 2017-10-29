package test.reflect;

import java.lang.annotation.ElementType;
import java.lang.reflect.Modifier;

public class Extraction {

	public static String classInfo(Class<?> clazz) {
		StringBuffer sb = new StringBuffer();
		
		sb.append(Modifier.toString(clazz.getModifiers()));
		sb.append(" ");
		sb.append(clazz.isInterface() ? "" : (clazz.isEnum() ? "enum " : "class "));
		sb.append(clazz.getName());
		sb.append(" ");
		sb.append("{}");
		
		
		return sb.toString();
	}

	
	public static void main(String[] args) {
		System.out.println(classInfo(ElementType.class));
	}
}
