package org.yuan.study.spring.core.annotation;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.WeakHashMap;

import org.yuan.study.spring.core.BridgeMethodResolver;
import org.yuan.study.spring.util.Assert;

public abstract class AnnotationUtils {

	private static final Map<Class<?>, Boolean> annotatedInterfaceCache = new WeakHashMap<Class<?>, Boolean>();
	
	/**
	 * Get a single 'Annotation' of 'annotationType' from the supplied 'Method'.
	 * @param method
	 * @param annotation
	 * @return
	 */
	public static <A extends Annotation> A getAnnotation(Method method, Class<A> annotationType) {
		Method resolvedMethod = BridgeMethodResolver.findBridgeMethod(method);
		A anno = resolvedMethod.getAnnotation(annotationType);
		if (anno == null) {
			for (Annotation metaAnno : resolvedMethod.getAnnotations()) {
				anno = metaAnno.annotationType().getAnnotation(annotationType);
				if (anno != null) {
					break;
				}
			}
		}
		return anno;
	}
	
	/**
	 * Get a single 'Annotation' of 'annotationType' from the supplied 'Method',
	 * traversing its super methods of no annotation can be found on the given method itself.
	 * @param method
	 * @param annotationType
	 * @return
	 */
	public static <A extends Annotation> A findAnnotation(Method method, Class<A> annotationType) {
		A annotation = getAnnotation(method, annotationType);
		Class<?> clazz = method.getDeclaringClass();
		if (annotation == null) {
			annotation = searchOnInterfaces(method, annotationType, clazz.getInterfaces());
		}
		while (annotation == null) {
			clazz = clazz.getSuperclass();
			if (clazz == null || clazz == Object.class) {
				break;
			}
			try {
				Method equivalentMethod = clazz.getDeclaredMethod(method.getName(), method.getParameterTypes());
				annotation = getAnnotation(equivalentMethod, annotationType);
				if (annotation == null) {
					annotation = searchOnInterfaces(method, annotationType, clazz.getInterfaces());
				}
			} 
			catch (NoSuchMethodException e) {
			}
		}
		return annotation;
	}
	
	private static <A extends Annotation> A searchOnInterfaces(Method method, Class<A> annotationType, Class<?>[] interfaces) {
		A annotation = null;
		for (Class<?> iface : interfaces) {
			if (isInterfaceWithAnnotatedMethods(iface)) {
				try {
					Method equivalentMethod = iface.getMethod(method.getName(), method.getParameterTypes());
					annotation = getAnnotation(equivalentMethod, annotationType);
				} 
				catch (NoSuchMethodException e) {}
				if (annotation != null) {
					break;
				}
			}
		}
		return annotation;
	}
	
	private static boolean isInterfaceWithAnnotatedMethods(Class<?> iface) {
		synchronized (annotatedInterfaceCache) {
			Boolean flag = annotatedInterfaceCache.get(iface);
			if (flag != null) {
				return flag;
			}
			boolean found = false;
			for (Method method : iface.getMethods()) {
				if (method.getAnnotations().length > 0) {
					found = true;
					break;
				}
			}
			annotatedInterfaceCache.put(iface, found);
			return found;
		}
	}
	
	/**
	 * Find a single 'Annotation' of 'annotationType' from the supplied 'Class',
	 * traversing its interfaces and superclasses if no annotation can be found
	 * on the given class itself.
	 * @param clazz
	 * @param annotationType
	 * @return
	 */
	public static <A extends Annotation> A findAnnotation(Class<?> clazz, Class<A> annotationType) {
		Assert.notNull(clazz, "Class must not be null");
		
		A annotation = clazz.getAnnotation(annotationType);
		if (annotation != null) {
			return annotation;
		}
		for (Class<?> iface : clazz.getInterfaces()) {
			annotation = findAnnotation(iface, annotationType);
			if (annotation != null) {
				return annotation;
			}
		}
		Class<?> superClass = clazz.getSuperclass();
		if (superClass == null || superClass == Object.class) {
			return null;
		}
		return findAnnotation(superClass, annotationType);
	}
}
