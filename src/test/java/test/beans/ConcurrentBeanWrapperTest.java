package test.beans;

import static org.junit.Assert.fail;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;

import org.junit.Test;
import org.yuan.study.spring.beans.BeanWrapperImpl;

public final class ConcurrentBeanWrapperTest {
	private Set<TestRun> set = Collections.synchronizedSet(new HashSet<TestRun>());
	private Throwable ex = null;

	@Test
	public void testSingleThread() {
		for (int i = 0; i < 100; i++) {
			performSet();
		}
	}
	
	@Test
	public void testConcurrent() {
		for (int i = 0; i < 10; i++) {
			TestRun run = new TestRun(this);
			set.add(run);
			Thread t = new Thread(run);
			t.setDaemon(true);
			t.start();
		}
		System.out.printf("Thread creation over, %s still active.\n", set.size());
		
		synchronized (this) {
			while (!set.isEmpty() && ex == null) {
				try {
					wait();
				}
				catch (InterruptedException ex) {
					System.out.print(ex.toString());
				}
				System.out.printf("%s threads still active.\n", set.size());
			}
		}
		
		if (ex != null) {
			fail(ex.getMessage());
		}
	}
	
	private static void performSet() {
		TestBean bean = new TestBean();
		
		Properties props = (Properties) System.getProperties().clone();
		assertTrue(props.size() != 0);
		Iterator<?> iter = props.entrySet().iterator();
		while (iter.hasNext()) {
			iter.next();
			if (Math.random() > 0.9) {
				iter.remove();
			}
		}
		
		ByteArrayOutputStream buf = new ByteArrayOutputStream();
		try {
			props.store(buf, null);
		}
		catch (IOException ex) {
			
		}
		String value = new String(buf.toByteArray());
		BeanWrapperImpl wrapper = new BeanWrapperImpl(bean);
		wrapper.setPropertyValue("properties", value);
		assertEquals(props, bean.getProperties());
	}
	
	private static class TestRun implements Runnable {
		private ConcurrentBeanWrapperTest test;
		
		public TestRun(ConcurrentBeanWrapperTest test) {
			this.test = test;
		}
		
		public void run() {
			try {
				for (int i = 0; i < 100; i++) {
					performSet();
				}
			}
			catch (Throwable ex) {
				test.ex = ex;
			}
			finally {
				synchronized (test) {
					test.set.remove(this);
					test.notifyAll();
				}
			}
		}
	}
	
	private static class TestBean {
		private Properties properties;

		public Properties getProperties() {
			return properties;
		}

		public void setProperties(Properties properties) {
			this.properties = properties;
		}
	}
}
