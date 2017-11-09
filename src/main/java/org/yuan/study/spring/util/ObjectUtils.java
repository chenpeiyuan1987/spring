package org.yuan.study.spring.util;

import java.lang.reflect.Array;
import java.util.Arrays;

public abstract class ObjectUtils {
	
	private static final int INITIAL_HASH = 7;
	private static final int MULTIPLIER = 31;
	
	private static final String EMPTY_STRING = "";
	private static final String NULL_STRING = "null";
	private static final String ARRAY_START = "{";
	private static final String ARRAY_END = "}";
	private static final String EMPTY_ARRAY = ARRAY_START + ARRAY_END;
	private static final String ARRAY_ELEMENT_SEPARATOR = ", ";
	
	/**
	 * Return whether the given throwable is a checked exception:
	 * that is, neither a RuntimeException nor an Error.
	 * @param ex
	 * @return
	 */
	public static boolean isCheckedException(Throwable ex) {
		return !(ex instanceof RuntimeException || ex instanceof Error);
	}
	
	/**
	 * Check whether the given exception is compatible with the exceptions
	 * declared in a throws clause.
	 * @param ex
	 * @param declaredExceptions
	 * @return
	 */
	public static boolean isCompatibleWithThrowsClause(Throwable ex, Class<?>[] declaredExceptions) {
		if (!isCheckedException(ex)) {
			return true;
		}
		if (declaredExceptions != null) {
			for (Class<?> dex : declaredExceptions) {
				if (dex.isAssignableFrom(ex.getClass())) {
					return true;
				}
			}
		}
		return false;
	}
	
	/**
	 * Determine whether the given object is an array:
	 * either an Object array or a primitive array.
	 * @param obj
	 * @return
	 */
	public static boolean isArray(Object obj) {
		return (obj != null && obj.getClass().isArray());
	}
	
	/**
	 * Return whether the given array is empty: 
	 * that is, null or of zero length.
	 * @param array
	 * @return
	 */
	public static boolean isEmpty(Object[] array) {
		return (array == null || array.length == 0);
	}
	
	/**
	 * Check whether the given array contains the given element.
	 * @param array
	 * @param element
	 * @return
	 */
	public static boolean containsElement(Object[] array, Object element) {
		if (array == null) {
			return false;
		}
		for (Object object : array) {
			if (nullSafeEquals(object, element)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Append the given Object to the given array, returning a new array
	 * consisting of the input array contents plus the given Object.
	 * @param array
	 * @param obj
	 * @return
	 */
	public static Object[] addObjectToArray(Object[] array, Object obj) {
		Class compType = Object.class;
		if (array != null) {
			compType = array.getClass().getComponentType();
		}
		else if (obj != null) {
			compType = obj.getClass();
		}
		int newArrLength = (array != null ? array.length + 1 : 1);
		Object[] newArr = (Object[]) Array.newInstance(compType, newArrLength);
		if (array != null) {
			System.arraycopy(array, 0, newArr, 0, array.length);
		}
		newArr[newArr.length - 1] = obj;
		return newArr;
	}
	
	/**
	 * Convert the given array to an object array.
	 * @param source
	 * @return
	 */
	public static Object[] toObjectArray(Object source) {
		if (source instanceof Object[]) {
			return (Object[]) source;
		}
		if (source == null) {
			return new Object[0];
		}
		if (!source.getClass().isArray()) {
			throw new IllegalArgumentException("Source is not an array: " + source);
		}
		int length = Array.getLength(source);
		if (length == 0) {
			return new Object[0];
		}
		Class wrapperType = Array.get(source, 0).getClass();
		Object[] newArray = (Object[])Array.newInstance(wrapperType, length);
		for (int i = 0; i < length; i++) {
			newArray[i] = Array.get(source, i);
		}
		return newArray;
	}
	
	/**
	 * Determine if the given objects are equal, 
	 * returning true if both are null or false if only one is null.
	 * @param o1
	 * @param o2
	 * @return
	 */
	public static boolean nullSafeEquals(Object o1, Object o2) {
		if (o1 == o2) {
			return true;
		}
		if (o1 == null || o2 == null) {
			return false;
		}
		if (o1.equals(o2)) {
			return true;
		}
		if (o1.getClass().isArray() && o2.getClass().isArray()) {
			if (o1 instanceof Object[] && o2 instanceof Object[]) {
				return Arrays.equals((Object[])o1, (Object[])o2);
			}
			if (o1 instanceof boolean[] && o2 instanceof boolean[]) {
				return Arrays.equals((boolean[])o1, (boolean[])o2);
			}
			if (o1 instanceof byte[] && o2 instanceof byte[]) {
				return Arrays.equals((byte[])o1, (byte[])o2);
			}
			if (o1 instanceof char[] && o2 instanceof char[]) {
				return Arrays.equals((char[])o1, (char[])o2);
			}
			if (o1 instanceof double[] && o2 instanceof double[]) {
				return Arrays.equals((double[])o1, (double[])o2);
			}
			if (o1 instanceof float[] && o2 instanceof float[]) {
				return Arrays.equals((float[])o1, (float[])o2);
			}
			if (o1 instanceof int[] && o2 instanceof int[]) {
				return Arrays.equals((int[])o1, (int[])o2);
			}
			if (o1 instanceof long[] && o2 instanceof long[]) {
				return Arrays.equals((long[])o1, (long[])o2);
			}
			if (o1 instanceof short[] && o2 instanceof short[]) {
				return Arrays.equals((short[])o1, (short[])o2);
			}
		}
		return false;
	}
	
	/**
	 * Return as hash code for the given object; typically the value of Object.hashCode().
	 * If array is null, this method returns 0.
	 * @param array
	 * @return
	 */
	public static int nullSafeHashCode(Object obj) {
		if (obj == null) {
			return 0;
		}
		if (obj.getClass().isArray()) {
			if (obj instanceof Object[]) {
				return nullSafeHashCode((Object[]) obj);
			}
			if (obj instanceof boolean[]) {
				return nullSafeHashCode((boolean[]) obj);
			}
			if (obj instanceof byte[]) {
				return nullSafeHashCode((byte[]) obj);
			}
			if (obj instanceof char[]) {
				return nullSafeHashCode((char[]) obj);
			}
			if (obj instanceof double[]) {
				return nullSafeHashCode((double[]) obj);
			}
			if (obj instanceof float[]) {
				return nullSafeHashCode((float[]) obj);
			}
			if (obj instanceof int[]) {
				return nullSafeHashCode((int[]) obj);
			}
			if (obj instanceof long[]) {
				return nullSafeHashCode((long[]) obj);
			}
			if (obj instanceof short[]) {
				return nullSafeHashCode((short[]) obj);
			}
		}
		return obj.hashCode();
	}
	
	/**
	 * Return a hash code based on the contents of the specified array.
	 * If array is null, this method returns 0.
	 * @param array
	 * @return
	 */
	public static int nullSafeHashCode(Object[] array) {
		if (array == null) {
			return 0;
		}
		int hash = INITIAL_HASH;
		int arraySize = array.length;
		for (int i = 0; i < arraySize; i++) {
			hash = MULTIPLIER * hash + nullSafeHashCode(array[i]);
		}
		return hash;
	}
	
	/**
	 * Return a hash code based on the contents of the specified array.
	 * If array is null, this method returns 0.
	 * @param array
	 * @return
	 */
	public static int nullSafeHashCode(boolean[] array) {
		if (array == null) {
			return 0;
		}
		int hash = INITIAL_HASH;
		int arraySize = array.length;
		for (int i = 0; i < arraySize; i++) {
			hash = MULTIPLIER * hash + hashCode(array[i]);
		}
		return hash;
	}
	
	/**
	 * Return a hash code based on the contents of the specified array.
	 * If array is null, this method returns 0.
	 * @param array
	 * @return
	 */
	public static int nullSafeHashCode(byte[] array) {
		if (array == null) {
			return 0;
		}
		int hash = INITIAL_HASH;
		int arraySize = array.length;
		for (int i = 0; i < arraySize; i++) {
			hash = MULTIPLIER * hash + array[i];
		}
		return hash;
	}
	
	/**
	 * Return a hash code based on the contents of the specified array.
	 * If array is null, this method returns 0.
	 * @param array
	 * @return
	 */
	public static int nullSafeHashCode(char[] array) {
		if (array == null) {
			return 0;
		}
		int hash = INITIAL_HASH;
		int arraySize = array.length;
		for (int i = 0; i < arraySize; i++) {
			hash = MULTIPLIER * hash + array[i];
		}
		return hash;
	}
	
	/**
	 * Return a hash code based on the contents of the specified array.
	 * If array is null, this method returns 0.
	 * @param array
	 * @return
	 */
	public static int nullSafeHashCode(double[] array) {
		if (array == null) {
			return 0;
		}
		int hash = INITIAL_HASH;
		int arraySize = array.length;
		for (int i = 0; i < arraySize; i++) {
			hash = MULTIPLIER * hash + hashCode(array[i]);
		}
		return hash;
	}
	
	/**
	 * Return a hash code based on the contents of the specified array.
	 * If array is null, this method returns 0.
	 * @param array
	 * @return
	 */
	public static int nullSafeHashCode(float[] array) {
		if (array == null) {
			return 0;
		}
		int hash = INITIAL_HASH;
		int arraySize = array.length;
		for (int i = 0; i < arraySize; i++) {
			hash = MULTIPLIER * hash + hashCode(array[i]);
		}
		return hash;
	}
	
	/**
	 * Return a hash code based on the contents of the specified array.
	 * If array is null, this method returns 0.
	 * @param array
	 * @return
	 */
	public static int nullSafeHashCode(int[] array) {
		if (array == null) {
			return 0;
		}
		int hash = INITIAL_HASH;
		int arraySize = array.length;
		for (int i = 0; i < arraySize; i++) {
			hash = MULTIPLIER * hash + array[i];
		}
		return hash;
	}
	
	/**
	 * Return a hash code based on the contents of the specified array.
	 * If array is null, this method returns 0.
	 * @param array
	 * @return
	 */
	public static int nullSafeHashCode(long[] array) {
		if (array == null) {
			return 0;
		}
		int hash = INITIAL_HASH;
		int arraySize = array.length;
		for (int i = 0; i < arraySize; i++) {
			hash = MULTIPLIER * hash + hashCode(array[i]);
		}
		return hash;
	}
	
	/**
	 * Return a hash code based on the contents of the specified array.
	 * If array is null, this method returns 0.
	 * @param array
	 * @return
	 */
	public static int nullSafeHashCode(short[] array) {
		if (array == null) {
			return 0;
		}
		int hash = INITIAL_HASH;
		int arraySize = array.length;
		for (int i = 0; i < arraySize; i++) {
			hash = MULTIPLIER * hash + array[i];
		}
		return hash;
	}
	
	/**
	 * Return the same value as Boolean.hashCode()
	 * @param flt
	 * @return
	 */
	public static int hashCode(boolean bool) {
		return bool ? 1231 : 1237;
	}
	
	/**
	 * Return the same value as Double.hashCode()
	 * @param flt
	 * @return
	 */
	public static int hashCode(double dbl) {
		long bits = Double.doubleToLongBits(dbl);
		return hashCode(bits);
	}

	/**
	 * Return the same value as Float.hashCode()
	 * @param flt
	 * @return
	 */
	public static int hashCode(float flt) {
		return Float.floatToIntBits(flt);
	}
	
	/**
	 * Return the same value as Long.hashCode()
	 * @param flt
	 * @return
	 */
	public static int hashCode(long lng) {
		return (int) (lng ^ (lng >>> 32));
	}
	
	/**
	 * Return a String representation of an object's overall identity.
	 * @param obj
	 * @return
	 */
	public static String identityToString(Object obj) {
		if (obj == null) {
			return EMPTY_STRING;
		}
		return obj.getClass().getName() + "@" + getIdentityHexString(obj);
	}
	
	/**
	 * Return a hex string form of an object's identity hash code.
	 * @param obj
	 * @return
	 */
	public static String getIdentityHexString(Object obj) {
		return Integer.toHexString(System.identityHashCode(obj));
	}
	
	/**
	 * Return a content-based String representation if obj is not null;
	 * otherwise returns an empty String.
	 * @param obj
	 * @return
	 */
	public static String getDisplayString(Object obj) {
		if (obj == null) {
			return EMPTY_STRING;
		}
		return nullSafeToString(obj);
	}
	
	/**
	 * Determine the class name for the given object.
	 * @param obj
	 * @return
	 */
	public static String nullSafeClassName(Object obj) {
		return (obj != null ? obj.getClass().getName() : NULL_STRING);
	}
	
	/**
	 * Return a String representation of the specified Object.
	 * @param obj
	 * @return
	 */
	public static String nullSafeToString(Object obj) {
		if (obj == null) {
			return NULL_STRING;
		}
		if (obj instanceof String) {
			return (String) obj;
		}
		if (obj instanceof Object[]) {
			return nullSafeToString((Object[]) obj);
		}
		if (obj instanceof boolean[]) {
			return nullSafeToString((boolean[]) obj);
		}
		if (obj instanceof byte[]) {
			return nullSafeToString((byte[]) obj);
		}
		if (obj instanceof char[]) {
			return nullSafeToString((char[]) obj);
		}
		if (obj instanceof double[]) {
			return nullSafeToString((double[]) obj);
		}
		if (obj instanceof float[]) {
			return nullSafeToString((float[]) obj);
		}
		if (obj instanceof int[]) {
			return nullSafeToString((int[]) obj);
		}
		if (obj instanceof long[]) {
			return nullSafeToString((long[]) obj);
		}
		if (obj instanceof short[]) {
			return nullSafeToString((short[]) obj);
		}
		String str = obj.toString();
		return (str != null ? str : EMPTY_ARRAY);
	}
	
	/**
	 * Return a String representation of the contents of the specified array.
	 * @param array
	 * @return
	 */
	public static String nullSafeToString(Object[] array) {
		if (array == null) {
			return NULL_STRING;
		}
		int length = array.length;
		if (length == 0) {
			return EMPTY_ARRAY;
		}
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < length; i++) {
			if (i == 0) {
				sb.append(ARRAY_START);
			} 
			else {
				sb.append(ARRAY_ELEMENT_SEPARATOR);
			}
			sb.append(String.valueOf(array[i]));
		}
		sb.append(ARRAY_END);
		return sb.toString();
	}
	
	/**
	 * Return a String representation of the contents of the specified array.
	 * @param array
	 * @return
	 */
	public static String nullSafeToString(boolean[] array) {
		if (array == null) {
			return NULL_STRING;
		}
		int length = array.length;
		if (length == 0) {
			return EMPTY_ARRAY;
		}
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < length; i++) {
			if (i == 0) {
				sb.append(ARRAY_START);
			} 
			else {
				sb.append(ARRAY_ELEMENT_SEPARATOR);
			}
			sb.append(array[i]);
		}
		sb.append(ARRAY_END);
		return sb.toString();
	}
	
	/**
	 * Return a String representation of the contents of the specified array.
	 * @param array
	 * @return
	 */
	public static String nullSafeToString(byte[] array) {
		if (array == null) {
			return NULL_STRING;
		}
		int length = array.length;
		if (length == 0) {
			return EMPTY_ARRAY;
		}
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < length; i++) {
			if (i == 0) {
				sb.append(ARRAY_START);
			} 
			else {
				sb.append(ARRAY_ELEMENT_SEPARATOR);
			}
			sb.append(array[i]);
		}
		sb.append(ARRAY_END);
		return sb.toString();
	}
	
	/**
	 * Return a String representation of the contents of the specified array.
	 * @param array
	 * @return
	 */
	public static String nullSafeToString(char[] array) {
		if (array == null) {
			return NULL_STRING;
		}
		int length = array.length;
		if (length == 0) {
			return EMPTY_ARRAY;
		}
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < length; i++) {
			if (i == 0) {
				sb.append(ARRAY_START);
			} 
			else {
				sb.append(ARRAY_ELEMENT_SEPARATOR);
			}
			sb.append(array[i]);
		}
		sb.append(ARRAY_END);
		return sb.toString();
	}
	
	/**
	 * Return a String representation of the contents of the specified array.
	 * @param array
	 * @return
	 */
	public static String nullSafeToString(double[] array) {
		if (array == null) {
			return NULL_STRING;
		}
		int length = array.length;
		if (length == 0) {
			return EMPTY_ARRAY;
		}
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < length; i++) {
			if (i == 0) {
				sb.append(ARRAY_START);
			} 
			else {
				sb.append(ARRAY_ELEMENT_SEPARATOR);
			}
			sb.append(array[i]);
		}
		sb.append(ARRAY_END);
		return sb.toString();
	}
	
	/**
	 * Return a String representation of the contents of the specified array.
	 * @param array
	 * @return
	 */
	public static String nullSafeToString(float[] array) {
		if (array == null) {
			return NULL_STRING;
		}
		int length = array.length;
		if (length == 0) {
			return EMPTY_ARRAY;
		}
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < length; i++) {
			if (i == 0) {
				sb.append(ARRAY_START);
			} 
			else {
				sb.append(ARRAY_ELEMENT_SEPARATOR);
			}
			sb.append(array[i]);
		}
		sb.append(ARRAY_END);
		return sb.toString();
	}
	
	/**
	 * Return a String representation of the contents of the specified array.
	 * @param array
	 * @return
	 */
	public static String nullSafeToString(int[] array) {
		if (array == null) {
			return NULL_STRING;
		}
		int length = array.length;
		if (length == 0) {
			return EMPTY_ARRAY;
		}
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < length; i++) {
			if (i == 0) {
				sb.append(ARRAY_START);
			} 
			else {
				sb.append(ARRAY_ELEMENT_SEPARATOR);
			}
			sb.append(array[i]);
		}
		sb.append(ARRAY_END);
		return sb.toString();
	}
	
	/**
	 * Return a String representation of the contents of the specified array.
	 * @param array
	 * @return
	 */
	public static String nullSafeToString(long[] array) {
		if (array == null) {
			return NULL_STRING;
		}
		int length = array.length;
		if (length == 0) {
			return EMPTY_ARRAY;
		}
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < length; i++) {
			if (i == 0) {
				sb.append(ARRAY_START);
			} 
			else {
				sb.append(ARRAY_ELEMENT_SEPARATOR);
			}
			sb.append(array[i]);
		}
		sb.append(ARRAY_END);
		return sb.toString();
	}
	
	/**
	 * Return a String representation of the contents of the specified array.
	 * @param array
	 * @return
	 */
	public static String nullSafeToString(short[] array) {
		if (array == null) {
			return NULL_STRING;
		}
		int length = array.length;
		if (length == 0) {
			return EMPTY_ARRAY;
		}
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < length; i++) {
			if (i == 0) {
				sb.append(ARRAY_START);
			} 
			else {
				sb.append(ARRAY_ELEMENT_SEPARATOR);
			}
			sb.append(array[i]);
		}
		sb.append(ARRAY_END);
		return sb.toString();
	}
	
}
