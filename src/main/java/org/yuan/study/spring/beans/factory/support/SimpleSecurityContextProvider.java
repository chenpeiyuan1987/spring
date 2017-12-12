package org.yuan.study.spring.beans.factory.support;

import java.security.AccessControlContext;
import java.security.AccessController;

public class SimpleSecurityContextProvider implements SecurityContextProvider {
	
	private final AccessControlContext acc;
	
	/**
	 * Construct a new 'SimpleSecurityContextProvider' instance.
	 */
	public SimpleSecurityContextProvider() {
		this(null);
	}

	/**
	 * Construct a new 'SimpleSecurityContextProvider' instance.
	 * @param acc
	 */
	public SimpleSecurityContextProvider(AccessControlContext acc) {
		this.acc = acc;
	}

	@Override
	public AccessControlContext getAccessControlContext() {
		return (acc != null ? acc : AccessController.getContext());
	}

}
