package org.yuan.study.spring;

import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.mockito.Mockito;



public class MockitoTest {

	@Test
	public void simpleTest() {
		List<String> list = Mockito.mock(List.class);
		
		//Mockito.when(list.get(0)).thenReturn("helloWorld");
		
		list.add("helloWorld");
		
		String result = list.get(0);
		
		Mockito.verify(list, Mockito.times(1)).get(0);
		
		Assert.assertEquals("helloWorld", result);
	}
}
