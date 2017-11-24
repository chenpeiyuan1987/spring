package org.yuan.study.spring.beans;

import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

public abstract class AbstractPropertyValuesTest {

	protected void doTestTony(PropertyValues pvs) throws Exception {
		assertTrue(pvs.getPropertyValues().length == 3);
		assertTrue(pvs.contains("forname"));
		assertTrue(pvs.contains("surname"));
		assertTrue(pvs.contains("age"));
		assertTrue(!pvs.contains("tory"));
		
		PropertyValue[] ps = pvs.getPropertyValues();
		Map<String, String> m = new HashMap<String, String>();
		m.put("forname", "Tony");
		m.put("surname", "Blair");
		m.put("age", "50");
		for (int i = 0; i < ps.length; i++) {
			Object val = m.get(ps[i].getName());
			assertTrue(val != null);
			assertTrue(val instanceof String);
			assertTrue(val.equals(ps[i].getValue()));
			m.remove(ps[i].getName());
		}
		assertTrue(m.size() == 0);
	}
}
