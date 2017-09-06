package org.yuan.study.spring.beans.propertyeditors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.junit.Test;

public class CustomCollectionEditorTest {

	@Test(expected=IllegalArgumentException.class)
	public void testCtorWithNullCollectionType() throws Exception {
		new CustomCollectionEditor(null);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testCtorWithNonCollectionType() throws Exception {
		new CustomCollectionEditor(String.class);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testWithCollectionTypeThatDoesNotExposeAPublicNoArgCtor() throws Exception {
		CustomCollectionEditor editor = new CustomCollectionEditor(CollectionTypeWithNoNoArgCtor.class);
		editor.setValue("1");
	}
	
	@Test
	public void testSunnyDaySetValue() throws Exception {
		CustomCollectionEditor editor = new CustomCollectionEditor(ArrayList.class);
		editor.setValue(new int[]{0,1,2});
		Object value = editor.getValue();
		assertNotNull(value);
		assertTrue(value instanceof ArrayList);
		List<?> list = (List<?>) value;
		assertEquals(3, list.size());
		assertEquals(0, list.get(0));
		assertEquals(1, list.get(1));
		assertEquals(2, list.get(2));
	}
	
	@Test
	public void testWhenTargetTypeIsExactlyTheCollectionInterfaceUsesFallbackCollectionType() throws Exception {
		CustomCollectionEditor editor = new CustomCollectionEditor(Collection.class);
		editor.setValue("0, 1, 2");
		Collection<?> value = (Collection<?>) editor.getValue();
		assertNotNull(value);
		assertEquals(1, value.size());
		assertEquals("0, 1, 2", value.iterator().next());
	}
	
	@Test
	public void testSunnyDaySetAsTextYieldsSingleValue() throws Exception {
		CustomCollectionEditor editor = new CustomCollectionEditor(ArrayList.class);
		editor.setValue("0, 1, 2");
		Object value = editor.getValue();
		assertNotNull(value);
		assertTrue(value instanceof ArrayList);
		List<?> list = (List<?>) value;
		assertEquals(1, list.size());
		assertEquals("0, 1, 2", list.get(0));
	}
	
	//----------------------------------------------------------
	// Class for test
	//----------------------------------------------------------
	
	private static final class CollectionTypeWithNoNoArgCtor extends ArrayList<Object> {
		public CollectionTypeWithNoNoArgCtor(String arg) {}
	}
}
