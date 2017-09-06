package test.beans;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;
import org.yuan.study.spring.beans.BeanWrapperImpl;

public final class BeanWrapperAutoGrowingTest {
	Bean bean = new Bean();
	BeanWrapperImpl wrapper = new BeanWrapperImpl(bean);
	
	@Before
	public void setUp() {
		//wrapper.set
	}
	
	@Test
	public void getPropertyValueNullValueInNestedPath() {
		assertNull(wrapper.getPropertyValue("nested.prop"));
	}
	
	
	
	//------------------------------------------
	// Class for test
	//------------------------------------------
	
	public static class Bean {
		private String prop;
		private Bean nested;
		private NestedNoDefaultConstructor nestedNoConstructor;
		private Bean[] array;
		private Bean[][] multiArray;
		private List<Bean> list;
		private List<List<Bean>> multiList;
		private List listNotParameterized;
		private Map<String,Bean> map;
		
		
		public String getProp() {
			return prop;
		}
		public void setProp(String prop) {
			this.prop = prop;
		}
		public Bean getNested() {
			return nested;
		}
		public void setNested(Bean nested) {
			this.nested = nested;
		}
		public NestedNoDefaultConstructor getNestedNoConstructor() {
			return nestedNoConstructor;
		}
		public void setNestedNoConstructor(
				NestedNoDefaultConstructor nestedNoConstructor) {
			this.nestedNoConstructor = nestedNoConstructor;
		}
		public Bean[] getArray() {
			return array;
		}
		public void setArray(Bean[] array) {
			this.array = array;
		}
		public Bean[][] getMultiArray() {
			return multiArray;
		}
		public void setMultiArray(Bean[][] multiArray) {
			this.multiArray = multiArray;
		}
		public List<Bean> getList() {
			return list;
		}
		public void setList(List<Bean> list) {
			this.list = list;
		}
		public List<List<Bean>> getMultiList() {
			return multiList;
		}
		public void setMultiList(List<List<Bean>> multiList) {
			this.multiList = multiList;
		}
		public List getListNotParameterized() {
			return listNotParameterized;
		}
		public void setListNotParameterized(List listNotParameterized) {
			this.listNotParameterized = listNotParameterized;
		}
		public Map<String, Bean> getMap() {
			return map;
		}
		public void setMap(Map<String, Bean> map) {
			this.map = map;
		}
		
	}
	
	public static class NestedNoDefaultConstructor {
		private NestedNoDefaultConstructor() {
		}
	}
}
