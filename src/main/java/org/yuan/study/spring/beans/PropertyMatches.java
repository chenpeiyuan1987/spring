package org.yuan.study.spring.beans;

import java.beans.PropertyDescriptor;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.yuan.study.spring.util.ObjectUtils;
import org.yuan.study.spring.util.StringUtils;

final class PropertyMatches {

	//--------------------------------------------
	// Static section
	//--------------------------------------------
	
	/** Default maximum property distance: 2 */
	public static final int DEFAULT_MAX_DISTANCE = 2;
	
	/**
	 * Create PropertyMatches for the given bean property.
	 * @param propertyName
	 * @param beanClass
	 * @return
	 */
	public static PropertyMatches forProperty(String propertyName, Class<?> beanClass) {
		return forProperty(propertyName, beanClass, DEFAULT_MAX_DISTANCE);
	}
	
	/**
	 * Create PropertyMatches for the given bean property.
	 * @param propertyName
	 * @param beanClass
	 * @param maxDistance
	 * @return
	 */
	public static PropertyMatches forProperty(String propertyName, Class<?> beanClass, int maxDistance) {
		return new PropertyMatches(propertyName, beanClass, maxDistance);
	}
	
	//--------------------------------------------
	// Instance section
	//--------------------------------------------
	
	private final String propertyName;
	
	private String[] possibleMatches;
	
	/**
	 * Create a new PropertyMatches instance for the given property.
	 * @param propertyName
	 * @param beanClass
	 * @param maxDistance
	 */
	private PropertyMatches(String propertyName, Class<?> beanClass, int maxDistance) {
		this.propertyName = propertyName;
		this.possibleMatches = calculateMatches(BeanUtils.getPropertyDescriptors(beanClass), maxDistance);
	}
	
	/**
	 * Build an error message for the given invalid property name,
	 * indicating the possible property matches.
	 * @return
	 */
	public String[] getPossibleMatches() {
		return possibleMatches;
	}
	
	/**
	 * Build an error message for the given invalid property name,
	 * indicating the possible property matches.
	 */
	public String buildErrorMessage() {
		StringBuilder msg = new StringBuilder();
		msg.append("Bean property '");
		msg.append(propertyName);
		msg.append("' is not writable or has an invalid setter method. ");
		
		if (ObjectUtils.isEmpty(possibleMatches)) {
			msg.append("Does the parameter type of the setter match the return type of the getter?");
		}
		else {
			msg.append("Did you mean ");
			for (int i = 0; i < possibleMatches.length; i++) {
				msg.append('\'');
				msg.append(possibleMatches[i]);
				if (i < possibleMatches.length - 2) {
					msg.append("', ");
				}
				else if (i == possibleMatches.length - 2) {
					msg.append("', or ");
				}
			}
			msg.append("'?");
			
		}
		
		return msg.toString();
	}
	
	/**
	 * Generate possible property alternatives for the given property and class.
	 * @param propertyDescriptors
	 * @param maxDistance
	 * @return
	 */
	private String[] calculateMatches(PropertyDescriptor[] propertyDescriptors, int maxDistance) {
		List<String> candidates = new ArrayList<String>();
		for (PropertyDescriptor propertyDescriptor : propertyDescriptors) {
			if (propertyDescriptor.getWriteMethod() != null) {
				String possibleAlternative = propertyDescriptor.getName();
				if (calculateStringDistance(propertyName, possibleAlternative) <= maxDistance) {
					candidates.add(possibleAlternative);
				}
			}
		}
		Collections.sort(candidates);
		return StringUtils.toStringArray(candidates);
	}
	
	/**
	 * Calculate the distance between the given two Strings.
	 * @param s1
	 * @param s2
	 * @return
	 */
	private int calculateStringDistance(String s1, String s2) {
		if (s1.length() == 0) {
			return s2.length();
		}
		if (s2.length() == 0) {
			return s1.length();
		}
		int[][] d = new int[s1.length() + 1][s2.length() + 1];
		
		for (int i = 0; i <= s1.length(); i++) {
			d[i][0] = i;
		}
		for (int i = 0; i < s2.length(); i++) {
			d[0][i] = i;
		}
		
		for (int i = 1; i < s1.length(); i++) {
			char si = s1.charAt(i - 1);
			for (int j = 1; j < s2.length(); j++) {
				int cost;
				char sj = s2.charAt(i - 1);
				if (si == sj) {
					cost = 0;
				} 
				else {
					cost = 1;
				}
				d[i][j] = Math.min(Math.min(d[i - 1][j] + 1, d[i][j - 1] + 1), d[i - 1][j - 1] + cost);
			}
		}
		
		return d[s1.length()][s2.length()];
	}
}
