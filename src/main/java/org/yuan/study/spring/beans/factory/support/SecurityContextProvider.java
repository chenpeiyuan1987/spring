package org.yuan.study.spring.beans.factory.support;

import java.security.AccessControlContext;

public interface SecurityContextProvider {

	/**
	 * Provides a security access control context relevant to a bean factory.
	 * @return
	 */
	AccessControlContext getAccessControlContext();
}
