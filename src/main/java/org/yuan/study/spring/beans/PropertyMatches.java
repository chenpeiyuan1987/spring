package org.yuan.study.spring.beans;

final class PropertyMatches {

	//--------------------------------------------
	// Static section
	//--------------------------------------------
	
	/** Default maximum property distance: 2 */
	public static final int DEFAULT_MAX_DISTANCE = 2;
	
	/**
	 * 
	 * @param propertyName
	 * @param beanClass
	 * @return
	 */
	public static PropertyMatches forProperty(String propertyName, Class<?> beanClass) {
		
	}
	
	/**
	 * 
	 * @param propertyName
	 * @param beanClass
	 * @param maxDistance
	 * @return
	 */
	public static PropertyMatches forProperty(String propertyName, Class<?> beanClass, int maxDistance) {
		
	}
	
	//--------------------------------------------
	// Instance section
	//--------------------------------------------
	
	private final String propertyName;
	
	private String[] possibleMatches;
	
	/**
	 * 
	 * @param propertyName
	 * @param beanClass
	 * @param maxDistance
	 */
	private PropertyMatches(String propertyName, Class<?> beanClass, int maxDistance) {
		
	}
	
	public String[] getPossibleMatches() {
		
	}
	
	public String buildErrorMessage() {
		
	}
	
	private String[] calculateMatches(PropertyDescriptor[] propertyDescriptors, int maxDistance) {
		
	}
	
	private int calculateStringDistance(String s1, String s2) {
		
	}
}
