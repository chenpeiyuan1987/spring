package org.yuan.study.spring.beans.factory;

import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.LinkedList;
import java.util.List;

import org.yuan.study.spring.beans.FatalBeanException;
import org.yuan.study.spring.core.NestedRuntimeException;

public class BeanCreationException extends FatalBeanException {
	private static final long serialVersionUID = 1L;
	
	private String beanName;
	
	private String resourceDescription;
	
	private List<Throwable> relatedCauses;

	/**
	 * Create a new BeanCreationException.
	 * @param message
	 */
	public BeanCreationException(String message) {
		super(message);
	}
	
	/**
	 * Create a new BeanCreationException.
	 * @param message
	 * @param cause
	 */
	public BeanCreationException(String message, Throwable cause) {
		super(message, cause);
	}
	
	/**
	 * Create a new BeanCreationException.
	 * @param beanName
	 * @param message
	 */
	public BeanCreationException(String beanName, String message) {
		super(String.format("Error creating bean with name '%s': %s", beanName, message));
		this.beanName = beanName;
	}
	
	/**
	 * Create a new BeanCreationException.
	 * @param beanName
	 * @param message
	 * @param cause
	 */
	public BeanCreationException(String beanName, String message, Throwable cause) {
		this(beanName, message);
		initCause(cause);
	}
	
	/**
	 * Create a new BeanCreationException.
	 * @param resourceDescription
	 * @param beanName
	 * @param message
	 */
	public BeanCreationException(String resourceDescription, String beanName, String message) {
		super(String.format("Error creating bean with name '%s'%s: %s", 
			beanName, (resourceDescription != null ? " defined in " + resourceDescription : ""), message));
		this.resourceDescription = resourceDescription;
		this.beanName = beanName;
	}
	
	/**
	 * Create a new BeanCreationException.
	 * @param resourceDescription
	 * @param beanName
	 * @param message
	 * @param cause
	 */
	public BeanCreationException(String resourceDescription, String beanName, String message, Throwable cause) {
		this(resourceDescription, beanName, message);
		initCause(cause);
	}

	/**
	 * Return the name of the bean requested, if any.
	 * @return
	 */
	public String getBeanName() {
		return beanName;
	}

	/**
	 * Return the description of the resource that the bean
	 * definition came from, if any.
	 * @return
	 */
	public String getResourceDescription() {
		return resourceDescription;
	}
	
	/**
	 * Add a related cause to this bean creation exception,
	 * not being a direct cause of the failure but having
	 * occured earlier in the creation of the same bean instance.
	 * @param ex
	 */
	public void addRelatedCause(Throwable ex) {
		if (relatedCauses == null) {
			relatedCauses = new LinkedList<Throwable>();
		}
		relatedCauses.add(ex);
	}

	/**
	 * Return the related causes, if any.
	 * @return
	 */
	public Throwable[] getRelatedCauses() {
		if (relatedCauses == null) {
			return null;
		}
		return relatedCauses.toArray(new Throwable[relatedCauses.size()]);
	}

	@Override
	public boolean contains(Class<?> exClass) {
		if (super.contains(exClass)) {
			return true;
		}
		if (relatedCauses != null) {
			for (Throwable throwable : relatedCauses) {
				if (throwable instanceof NestedRuntimeException 
					&& ((NestedRuntimeException) throwable).contains(exClass)) {
					return true;
				}
			}
		}
		return false;
	}

	@Override
	public void printStackTrace(PrintStream ps) {
		synchronized (ps) {
			super.printStackTrace(ps);
			if (relatedCauses != null) {
				for (Throwable throwable : relatedCauses) {
					ps.println("Related cause:");
					throwable.printStackTrace(ps);
				}
			}
		}
	}

	@Override
	public void printStackTrace(PrintWriter pw) {
		synchronized (pw) {
			super.printStackTrace(pw);
			if (relatedCauses != null) {
				for (Throwable throwable : relatedCauses) {
					pw.println("Related cause:");
					throwable.printStackTrace(pw);
				}
			}
		}
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder(super.toString());
		if (relatedCauses != null) {
			for (Throwable relatedCause : relatedCauses) {
				sb.append("\nRelated cause: ");
				sb.append(relatedCause);
			}
		}
		return sb.toString();
	}
	
}
