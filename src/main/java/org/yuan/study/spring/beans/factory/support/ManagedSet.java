package org.yuan.study.spring.beans.factory.support;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;


public class ManagedSet<E> implements Set<E> {
	
	private final Set<E> set;
	
	public ManagedSet() {
		this(16);
	}
	
	public ManagedSet(int initialCapacity) {
		this.set = new LinkedHashSet<E>();
	}

	public ManagedSet(Set<E> set) {
		this.set = set;
	}
	
	//---------------------------------------------------
	// Implementation of Set interface
	//---------------------------------------------------
	
	@Override
	public int size() {
		return set.size();
	}

	@Override
	public boolean isEmpty() {
		return set.isEmpty();
	}

	@Override
	public boolean contains(Object o) {
		return set.contains(o);
	}

	@Override
	public Iterator<E> iterator() {
		return set.iterator();
	}

	@Override
	public Object[] toArray() {
		return set.toArray();
	}

	@Override
	public <T> T[] toArray(T[] a) {
		return set.toArray(a);
	}

	@Override
	public boolean add(E e) {
		return set.add(e);
	}

	@Override
	public boolean remove(Object o) {
		return set.remove(o);
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		return set.containsAll(c);
	}

	@Override
	public boolean addAll(Collection<? extends E> c) {
		return set.addAll(c);
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		return set.retainAll(c);
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		return set.removeAll(c);
	}

	@Override
	public void clear() {
		set.clear();
	}

	//--------------------------------------------------------------
	// Implementation of other methods
	//--------------------------------------------------------------
	
	@Override
	public int hashCode() {
		return set.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		return set.equals(obj);
	}

	@Override
	public String toString() {
		return set.toString();
	}
}
