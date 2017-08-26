package test.beans;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeSet;

public class IndexedTestBean {
	private TestBean[] array;
	private Collection<Object> collection;
	private List<Object> list;
	private Set<Object> set;
	private SortedSet<Object> sortedSet;
	private Map<Object,Object> map;
	private SortedMap<Object,Object> sortedMap;
	
	
	public IndexedTestBean() {
		this(true);
	}
	public IndexedTestBean(boolean populate) {
		if (populate) {
			populate();
		}
	}
	
	public void populate() {
		TestBean tb0 = new TestBean("name0", 0);
		TestBean tb1 = new TestBean("name1", 0);
		TestBean tb2 = new TestBean("name2", 0);
		TestBean tb3 = new TestBean("name3", 0);
		TestBean tb4 = new TestBean("name4", 0);
		TestBean tb5 = new TestBean("name5", 0);
		TestBean tb6 = new TestBean("name6", 0);
		TestBean tb7 = new TestBean("name7", 0);
		TestBean tb8 = new TestBean("name8", 0);
		TestBean tb9 = new TestBean("name9", 0);
		
		array = new TestBean[]{tb0, tb1};
		list = new ArrayList<Object>();
		list.add(tb2);
		list.add(tb3);
		set = new TreeSet<Object>();
		set.add(tb6);
		set.add(tb7);
		map = new HashMap<Object,Object>();
		map.put("key1", tb4);
		map.put("key2", tb5);
		map.put("key.3", tb5);
		List<Object> list1 = new ArrayList<Object>();
		list1.add(tb8);
		list1.add(tb9);
		map.put("key4", list);
	}
	
	public TestBean[] getArray() {
		return array;
	}
	public void setArray(TestBean[] array) {
		this.array = array;
	}
	public Collection<?> getCollection() {
		return collection;
	}
	public void setCollection(Collection<Object> collection) {
		this.collection = collection;
	}
	public List<Object> getList() {
		return list;
	}
	public void setList(List<Object> list) {
		this.list = list;
	}
	public Set<Object> getSet() {
		return set;
	}
	public void setSet(Set<Object> set) {
		this.set = set;
	}
	public SortedSet<Object> getSortedSet() {
		return sortedSet;
	}
	public void setSortedSet(SortedSet<Object> sortedSet) {
		this.sortedSet = sortedSet;
	}
	public Map<Object, Object> getMap() {
		return map;
	}
	public void setMap(Map<Object, Object> map) {
		this.map = map;
	}
	public SortedMap<Object, Object> getSortedMap() {
		return sortedMap;
	}
	public void setSortedMap(SortedMap<Object, Object> sortedMap) {
		this.sortedMap = sortedMap;
	}
	
}
