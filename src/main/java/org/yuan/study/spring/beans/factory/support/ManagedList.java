package org.yuan.study.spring.beans.factory.support;

import java.util.ArrayList;

public class ManagedList<E> extends ArrayList<E> {
	private static final long serialVersionUID = 1L;

	public ManagedList() {
	}

	public ManagedList(int initialCapacity) {
		super(initialCapacity);
	}
	
}
