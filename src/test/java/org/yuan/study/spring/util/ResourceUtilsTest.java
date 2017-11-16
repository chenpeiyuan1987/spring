package org.yuan.study.spring.util;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class ResourceUtilsTest {

	@Test
	public void testIsJarURL() throws Exception {
		assertTrue(ResourceUtils.isJarURL(new URL("jar:file:myjar.jar!/mypath")));
		assertTrue(ResourceUtils.isJarURL(new URL(null, "zip:file:myjar.jar!/mypath", new DummyURLStreamHandler())));
		assertTrue(ResourceUtils.isJarURL(new URL(null, "wsjar:file:myjar.jar!/mypath", new DummyURLStreamHandler())));
		assertFalse(ResourceUtils.isJarURL(new URL("file:myjar.jar")));
		assertFalse(ResourceUtils.isJarURL(new URL("http:myserver/myjar.jar")));
	}
	
	@Test
	public void testExtractJarFileURL() throws Exception {
		assertEquals(new URL("file:myjar.jar"),
			ResourceUtils.extractJarFileURL(new URL("jar:file:myjar.jar!/mypath")));
		assertEquals(new URL("file:/myjar.jar"),
				ResourceUtils.extractJarFileURL(new URL(null, "jar:myjar.jar!/mypath", new DummyURLStreamHandler())));
		assertEquals(new URL("file:myjar.jar"),
				ResourceUtils.extractJarFileURL(new URL(null, "zip:file:myjar.jar!/mypath", new DummyURLStreamHandler())));
		assertEquals(new URL("file:myjar.jar"),
				ResourceUtils.extractJarFileURL(new URL(null, "wsjar:file:myjar.jar!/mypath", new DummyURLStreamHandler())));
		assertEquals(new URL("file:myjar.jar"),
				ResourceUtils.extractJarFileURL(new URL("jar:file:myjar.jar!/")));
		assertEquals(new URL("file:myjar.jar"),
				ResourceUtils.extractJarFileURL(new URL(null, "zip:file:myjar.jar!/", new DummyURLStreamHandler())));
		assertEquals(new URL("file:myjar.jar"),
				ResourceUtils.extractJarFileURL(new URL(null, "wsjar:file:myjar.jar!/", new DummyURLStreamHandler())));
		assertEquals(new URL("file:myjar.jar"),
				ResourceUtils.extractJarFileURL(new URL("file:myjar.jar")));
	}
	
	/**
	 * Dummy URLStreamHandler that's just specified to suppress the standard
	 * java.net.URL URLStreamHandler lookup, to be able to use the standard
	 * URL class for parsing "rmi:..." URLs.
	 */
	private static class DummyURLStreamHandler extends URLStreamHandler {

		@Override
		protected URLConnection openConnection(URL u) throws IOException {
			return null;
		}
		
	}
}
