package org.yuan.study.spring.beans;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

public final class MutablePropertyValuesTest {

	@Test
	public void testValid() throws Exception {
		MutablePropertyValues pvs = getTestTony();
		doTestTony(pvs);
		
		MutablePropertyValues deepCopy = new MutablePropertyValues(pvs);
		doTestTony(deepCopy);
		deepCopy.setPropertyValueAt(new PropertyValue("name", "Gordon"), 0);
		doTestTony(pvs);
		assertEquals("Gordon", deepCopy.getPropertyValue("name").getValue());
	}
	
	@Test
	public void testAddOrOverride() throws Exception {
		MutablePropertyValues pvs = getTestTony();
		doTestTony(pvs);
		
		PropertyValue addedPv = new PropertyValue("rod", "Rod");
		pvs.addPropertyValue(addedPv);
		assertTrue(pvs.getPropertyValue("rod").equals(addedPv));
		
		PropertyValue changedPv = new PropertyValue("forname", "Greg");
		pvs.addPropertyValue(changedPv);
		assertTrue(pvs.getPropertyValue("forname").equals(changedPv));
	}
	
	@Test
	public void testChangesOnEquals() throws Exception {
		MutablePropertyValues pvs = getTestTony();
		doTestTony(pvs);
		
		PropertyValues changes = pvs.changesSince(pvs);
		assertTrue(changes.getPropertyValues().length == 0);
		
		MutablePropertyValues pvs2 = new MutablePropertyValues();
		changes = pvs.changesSince(pvs2);
		assertEquals(3, changes.getPropertyValues().length);
	}
	
	@Test
	public void testChangeOfOneField() throws Exception {
		MutablePropertyValues pvs = getTestTony();
		doTestTony(pvs);
		
		MutablePropertyValues pvs2 = new MutablePropertyValues(pvs);
		PropertyValues changes = pvs2.changesSince(pvs);
		assertTrue(changes.getPropertyValues().length == 0);
		
		pvs2.addPropertyValue(new PropertyValue("forname", "Gordon"));
		changes = pvs2.changesSince(pvs);
		assertEquals(1, changes.getPropertyValues().length);
		PropertyValue pv = changes.getPropertyValue("forname");
		assertNotNull(pv);
		assertTrue(pv.getValue().equals("Gordon"));
		
		MutablePropertyValues pvs3 = new MutablePropertyValues(pvs);
		changes = pvs3.changesSince(pvs);
		assertEquals(0, changes.getPropertyValues().length);
		
		pvs3.addPropertyValue(new PropertyValue("foo", "bar"));
		pvs3.addPropertyValue(new PropertyValue("fi", "fum"));
		changes = pvs3.changesSince(pvs);
		assertEquals(2, changes.getPropertyValues().length);
		pv = changes.getPropertyValue("foo");
		assertNotNull(pv);
		assertEquals("bar", pv.getValue());
	}
	
	private MutablePropertyValues getTestTony() {
		MutablePropertyValues pvs = new MutablePropertyValues();
		pvs.addPropertyValue(new PropertyValue("forname", "Tony"));
		pvs.addPropertyValue(new PropertyValue("surname", "Blair"));
		pvs.addPropertyValue(new PropertyValue("age", "50"));
		
		return pvs;
	}
	
	private void doTestTony(PropertyValues pvs) throws Exception {
		assertEquals(3, pvs.getPropertyValues().length);
		assertTrue(pvs.contains("forname"));
		assertTrue(pvs.contains("surname"));
		assertTrue(pvs.contains("age"));
		assertFalse(pvs.contains("tory"));
		
		PropertyValue[] ps = pvs.getPropertyValues();
		Map<String,String> m = new HashMap<String,String>();
		m.put("forname", "Tony");
		m.put("surname", "Blair");
		m.put("age", "50");
		for (PropertyValue pv : ps) {
			Object val = m.get(pv.getName());
			assertNotNull(val);
			assertTrue(val instanceof String);
			assertEquals(pv.getValue(), val);
			m.remove(pv.getName());
		}
		assertEquals(0, m.size());
	}
}
