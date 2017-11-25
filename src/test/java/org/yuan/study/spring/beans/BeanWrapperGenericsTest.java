package org.yuan.study.spring.beans;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Test;
import org.yuan.study.spring.beans.propertyeditors.CustomNumberEditor;
import org.yuan.study.spring.core.io.Resource;
import org.yuan.study.spring.core.io.UrlResource;

import test.beans.GenericBean;
import test.beans.TestBean;

public final class BeanWrapperGenericsTest {

	@Test
	public void testGenericSet() {
		GenericBean<?> gb = new GenericBean<Object>();
		BeanWrapper bw = new BeanWrapperImpl(gb);
		Set<String> input = new HashSet<String>();
		input.add("4");
		input.add("5");
		bw.setPropertyValue("integerSet", input);
		assertTrue(gb.getIntegerSet().contains(new Integer(4)));
		assertTrue(gb.getIntegerSet().contains(new Integer(5)));
	}
	
	@Test
	public void testGenericLowerBoundedSet() {
		GenericBean<?> gb = new GenericBean<Object>();
		BeanWrapper bw = new BeanWrapperImpl(gb);
		bw.registerCustomEditor(Number.class, new CustomNumberEditor(Integer.class, true));
		Set<String> input = new HashSet<String>();
		input.add("4");
		input.add("5");
		bw.setPropertyValue("numberSet", input);
		assertTrue(gb.getNumberSet().contains(new Integer(4)));
		assertTrue(gb.getNumberSet().contains(new Integer(5)));
	}
	
	@Test(expected=TypeMismatchException.class)
	public void testGenericSetWithConversionFailure() {
		GenericBean<?> gb = new GenericBean<Object>();
		BeanWrapper bw = new BeanWrapperImpl(gb);
		Set<TestBean> input = new HashSet<TestBean>();
		input.add(new TestBean());
		bw.setPropertyValue("integerSet", input);
	}
	
	@Test
	public void testGenericList() throws MalformedURLException {
		GenericBean<?> gb = new GenericBean<Object>();
		BeanWrapper bw = new BeanWrapperImpl(gb);
		List<String> input = new ArrayList<String>();
		input.add("http://localhost:8080");
		input.add("http://localhost:9090");
		bw.setPropertyValue("resourceList", input);
		assertEquals(new UrlResource("http://localhost:8080"), gb.getResourceList().get(0));
		assertEquals(new UrlResource("http://localhost:8080"), gb.getResourceList().get(0));
	}
	
	@Test
	public void testGenericListElement() throws MalformedURLException {
		GenericBean<?> gb = new GenericBean<Object>();
		gb.setResourceList(new ArrayList<Resource>());
		BeanWrapper bw = new BeanWrapperImpl(gb);
		bw.setPropertyValue("resourceList[0]", "http://localhost:8080");
		assertEquals(new UrlResource("http://localhost:8080"), gb.getResourceList().get(0));
	}
	
	@Test
	public void testGenericMap() {
		GenericBean<?> gb = new GenericBean<Object>();
		BeanWrapper bw = new BeanWrapperImpl(gb);
		Map<String, String> input = new HashMap<String, String>();
		input.put("4", "5");
		input.put("6", "7");
		bw.setPropertyValue("shortMap", input);
		assertEquals(new Integer(5), gb.getShortMap().get(new Short("4")));
		assertEquals(new Integer(7), gb.getShortMap().get(new Short("6")));
	}
	
	@Test
	public void testGenericMapElement() {
		GenericBean<?> gb = new GenericBean<Object>();
		gb.setShortMap(new HashMap<Short, Integer>());
		BeanWrapper bw = new BeanWrapperImpl(gb);
		bw.setPropertyValue("shortMap[4]", "5");
		assertEquals(new Integer(5), bw.getPropertyValue("shortMap[4]"));
		assertEquals(new Integer(5), gb.getShortMap().get(new Short("4")));
	}
	
	@Test
	public void testGenericMapWithKeyType() {
		GenericBean<?> gb = new GenericBean<Object>();
		BeanWrapper bw = new BeanWrapperImpl(gb);
		Map<String, String> input = new HashMap<String, String>();
		input.put("4", "5");
		input.put("6", "7");
		bw.setPropertyValue("longMap", input);
		assertEquals("5", gb.getLongMap().get(new Long("4")));
		assertEquals("7", gb.getLongMap().get(new Long("6")));
	}
	
	@Test
	public void testGenericMapElementWithKeyType() {
		GenericBean<?> gb = new GenericBean<Object>();
		gb.setLongMap(new HashMap<Long, Integer>());
		BeanWrapper bw = new BeanWrapperImpl(gb);
		bw.setPropertyValue("longMap[4]", "5");
		assertEquals("5", gb.getLongMap().get(new Long("4")));
		assertEquals("5", bw.getPropertyValue("longMap[4]"));
	}
	
	@Test
	public void testGenericMapWithCollectionValue() {
		GenericBean<?> gb = new GenericBean<Object>();
		BeanWrapper bw = new BeanWrapperImpl(gb);
		bw.registerCustomEditor(Number.class, new CustomNumberEditor(Integer.class, false));
		Map<String, Collection> input = new HashMap<String, Collection>();
		HashSet<Integer> value1 = new HashSet<Integer>();
		value1.add(new Integer(1));
		input.put("1", value1);
		ArrayList<Boolean> value2 = new ArrayList<Boolean>();
		value2.add(Boolean.TRUE);
		input.put("2", value2);
		bw.setPropertyValue("collectionMap", input);
		assertTrue(gb.getCollectionMap().get(new Integer(1)) instanceof HashSet);
		assertTrue(gb.getCollectionMap().get(new Integer(2)) instanceof ArrayList);
	}
	
	@Test
	public void testGenericMapElementWithCollectionValue() {
		GenericBean<?> gb = new GenericBean<Object>();
		gb.setCollectionMap(new HashMap<Number, Collection<? extends Object>>());
		BeanWrapper bw = new BeanWrapperImpl(gb);
		bw.registerCustomEditor(Number.class, new CustomNumberEditor(Integer.class, false));
		HashSet<Integer> value1 = new HashSet<Integer>();
		value1.add(new Integer(1));
		bw.setPropertyValue("collectionMap[1]", value1);
		assertTrue(gb.getCollectionMap().get(new Integer(1)) instanceof HashSet);
	}
	
	@Test
	public void testGenericListOfLists() {
		GenericBean<?> gb = new GenericBean<Object>();
		List<List<Integer>> list = new LinkedList<List<Integer>>();
		list.add(new LinkedList<Integer>());
		gb.setListOfLists(list);
		BeanWrapper bw = new BeanWrapperImpl(gb);
		bw.setPropertyValue("listOfLists[0][0]", new Integer(5));
		assertEquals(new Integer(5), gb.getListOfLists().get(0).get(0));
		assertEquals(new Integer(5), bw.getPropertyValue("listOfLists[0][0]"));
	}
	
	@Test
	public void testGenericListOfListsWithElementConversion() throws MalformedURLException {
		GenericBean<?> gb = new GenericBean<Object>();
		List<List<Integer>> list = new LinkedList<List<Integer>>();
		list.add(new LinkedList<Integer>());
		gb.setListOfLists(list);
		BeanWrapper bw = new BeanWrapperImpl(gb);
		bw.setPropertyValue("listOfLists[0][0]", "5");
		assertEquals(new Integer(5), gb.getListOfLists().get(0).get(0));
		assertEquals(new Integer(5), bw.getPropertyValue("listOfLists[0][0]"));
	}
	
	@Test
	public void testGenericListOfArrays() throws MalformedURLException {
		
	}
	
	//------------------------------------------------------------------
	// class section
	//------------------------------------------------------------------
}
