package org.yuan.study.spring.core.io;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

import org.springframework.util.Assert;

public class ByteArrayResource extends AbstractResource {
	
	private final byte[] byteArray;
	
	private final String description;
	
	/**
	 * Create a new ByteArrayResource
	 * @param byteArray
	 */
	public ByteArrayResource(byte[] byteArray) {
		this(byteArray, "resource loaded from byte array");
	}

	/**
	 * Create a new ByteArrayResource
	 * @param byteArray
	 * @param description
	 */
	public ByteArrayResource(byte[] byteArray, String description) {
		Assert.notNull(byteArray, "Byte array must not be null");
		this.byteArray = byteArray;
		this.description = (description != null ? description : "");
	}

	/**
	 * Return the underlying byte array.
	 * @return
	 */
	public byte[] getByteArray() {
		return byteArray;
	}
	
	//-------------------------------------------------------------
	// Implementation of AbstractResource class
	//-------------------------------------------------------------
	
	@Override
	public String getDescription() {
		return description;
	}

	@Override
	public InputStream getInputStream() throws IOException {
		return new ByteArrayInputStream(byteArray);
	}

	@Override
	public boolean exists() {
		return true;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == this) {
			return true;
		}
		if (obj instanceof ByteArrayResource) {
			ByteArrayResource other = (ByteArrayResource) obj;
			return Arrays.equals(byteArray, other.byteArray);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return (byte[].class.hashCode() * 29 * this.byteArray.length);
	}

}
