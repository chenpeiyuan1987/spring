package test.beans;

import java.io.Serializable;

import org.yuan.study.spring.beans.factory.BeanNameAware;
import org.yuan.study.spring.beans.factory.DisposableBean;

public class DerivedTestBean extends TestBean implements BeanNameAware, DisposableBean, Serializable {
	private static final long serialVersionUID = 1L;
	
	private String beanName;
	private boolean initialized;
	private boolean destroyed;
	
	public DerivedTestBean() {
	}
	public DerivedTestBean(String[] names) {
		if (names == null || names.length < 2) {
			throw new IllegalArgumentException("Invalid names array");
		}
		setName(names[0]);
		setBeanName(names[1]);
	}
	public static DerivedTestBean create(String[] names) {
		return new DerivedTestBean(names);
	}
	
	public void setActualSpouse(TestBean spouse) {
		setSpouse(spouse);
	}
	public void setSpouseRef(String name) {
		setSpouse(new TestBean(name));
	}
	public String getBeanName() {
		return beanName;
	}
	public void setBeanName(String beanName) {
		if (this.beanName == null || beanName == null) {
			this.beanName = beanName;
		}
	}
	public boolean wasInitialized() {
		return initialized;
	}
	public void initialize() {
		this.initialized = true;
	}
	public boolean wasDestroyed() {
		return destroyed;
	}
	public void destroy() {
		this.destroyed = true;
	}
	
}
