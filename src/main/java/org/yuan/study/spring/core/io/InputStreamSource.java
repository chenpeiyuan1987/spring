package org.yuan.study.spring.core.io;

import java.io.IOException;
import java.io.InputStream;

public interface InputStreamSource {

	/**
	 * Return an InputStream
	 * @return
	 */
	InputStream getInputStream() throws IOException;
	
}
