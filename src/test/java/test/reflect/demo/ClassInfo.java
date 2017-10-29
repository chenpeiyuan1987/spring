package test.reflect.demo;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.lang.reflect.TypeVariable;

public class ClassInfo {
	
	// 缩进空格
	private static int indentBlank = 4;
	//
	private static boolean fullName = true;

	public static String getClassInfo(Class<?> clazz) {
		return getClassInfo(clazz, 0);
	}
	
	public static String getClassInfo(Class<?> clazz, int level) {
		StringBuffer sb = new StringBuffer();
		String indentStr = getIndentStr(level);
		
		sb.append(indentStr).append(getModifiers(clazz)).append(" ");
		sb.append(indentStr).append(getClassName(clazz, isFullName())).append(" ");
		sb.append(indentStr).append("{").append("\n");
		
		String indentStr2 = getIndentStr(level + 1);
		
		// 字段
		for (Field field : clazz.getDeclaredFields()) {
			sb.append(indentStr2).append(getFieldInfo(field)).append("\n");
		}
		
		// 方法
		for (Method method : clazz.getDeclaredMethods()) {
			sb.append(indentStr2).append(getMethodInfo(method)).append("\n");
		}
		
		sb.append(indentStr).append("}");
		
		return sb.toString();
	}
	
	public static String getMethodInfo(Method method) {
		StringBuffer sb = new StringBuffer();
		
		// 修饰符
		sb.append(Modifier.toString(method.getModifiers()));
		if (method.getModifiers() > 0) {
			sb.append(" ");
		}
		
		// 返回类型
		sb.append(getClassName(method.getReturnType(), isFullName())).append(" ");
		
		// 方法名
		sb.append(method.getName());
		
		// 参数类型
		sb.append("(");
		if (method.getParameterCount() > 0) {
			for (Parameter param : method.getParameters()) {
				sb.append(getClassName(param.getType(), isFullName()) + " " + param.getName()).append(", ");
			}
			sb.delete(sb.length()-2, sb.length());
		}
		sb.append(") {}");
		
		return sb.toString();
	}
	
	public static String getFieldInfo(Field field) {
		StringBuffer sb = new StringBuffer();
		
		// 修饰符
		sb.append(Modifier.toString(field.getModifiers()));
		if (field.getModifiers() > 0) {
			sb.append(" ");
		}
		
		// 参数类型
		sb.append(getClassName(field.getType(), isFullName())).append(" ");
		
		// 参数名
		sb.append(field.getName());
		if (Modifier.isFinal(field.getModifiers()) && Modifier.isStatic(field.getModifiers())) {
			field.setAccessible(true);
			try {
				Object value = field.get(null);
				sb.append(" = ").append(value);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		sb.append(";");
		
		return sb.toString();
	}
	
	public static int getIndentBlank() {
		return indentBlank;
	}

	public static void setIndentBlank(int indentBlank) {
		ClassInfo.indentBlank = indentBlank;
	}

	public static boolean isFullName() {
		return fullName;
	}

	public static void setFullName(boolean fullName) {
		ClassInfo.fullName = fullName;
	}
	
	//
	private static String getModifiers(Class<?> clazz) {
		int modifiers = clazz.getModifiers();

		String result = Modifier.toString(modifiers);
		
		if (clazz.isInterface()) {
			result = result.replace("abstract interface", "interface");
		}
		else if (clazz.isEnum()) {
			result = result.replace("final", "enum");
		}
		else {
			result += " class";
		}
		
		return result;
	}
	
	//
	private static String getClassName(Class<?> clazz, boolean fullName) {
		StringBuffer sb = new StringBuffer();
		sb.append(fullName ? clazz.getName() : clazz.getSimpleName());
		
		TypeVariable[] types = clazz.getTypeParameters();
		if (types.length > 0) {
			sb.append("<");
			for (TypeVariable type : types) {
				sb.append(type.getName()).append(", ");
			}
			sb.delete(sb.length()-2, sb.length());
			sb.append(">");
		}
		
		return sb.toString();
	}
	
	//
	private static String getIndentStr(int level) {
		StringBuffer sb = new StringBuffer();
		
		int length = indentBlank * level;
		for (int i = 0; i < length; i++) {
			sb.append(' ');
		}
		
		return sb.toString();
	}

}
