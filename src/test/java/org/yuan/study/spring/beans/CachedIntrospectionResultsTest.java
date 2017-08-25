package org.yuan.study.spring.beans;

import org.junit.Test;
import static org.junit.Assert.*;

import test.beans.TestBean;

public final class CachedIntrospectionResultsTest {

	@Test
	public void testAcceptClassLoader() throws Exception {
		Object cache = CachedIntrospectionResults.forClass(TestBean.class);
		BeanWrapper bw = new BeanWrapperImpl(TestBean.class);
		assertTrue(bw.isWritableProperty("name"));
		assertTrue(bw.isWritableProperty("age"));
		assertEquals(cache, CachedIntrospectionResults.forClass(TestBean.class));
	}
}
