package test.beans;

import java.io.IOException;
import java.time.chrono.ThaiBuddhistChronology;
import java.util.Properties;

import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.BeanFactoryAware;
import org.yuan.study.spring.beans.factory.BeanNameAware;
import org.yuan.study.spring.util.ObjectUtils;

public class TestBean implements BeanNameAware, BeanFactoryAware, ITestBean, IOther , Comparable {

	private int age;
	private String name;
	private String touchy;
	
	private ITestBean[] spouses;
	private Properties properties = new Properties();
	
	private INestedTestBean doctor = new NestedTestBean();
	
	private INestedTestBean lawyer = new NestedTestBean();
	
	public TestBean() {
	}
	public TestBean(String name) {
		this.name = name;
	}
	public TestBean(String name, int age) {
		this.name = name;
		this.age = age;
	}
	public TestBean(ITestBean spouse) {
		this.spouses = new ITestBean[] {spouse};
	}
	public TestBean(ITestBean spouse, Properties properties) {
		this.spouses = new ITestBean[] {spouse};
		this.properties = properties;
	}

	public String getName() {
		return name;
	}
	
	public void setName(String name) {
		this.name = name;
	}
	
	public int getAge() {
		return age;
	}
	
	public void setAge(int age) {
		this.age = age;
	}
	
	public String getTouchy() {
		return touchy;
	}
	
	public void setTouchy(String touchy) {
		this.touchy = touchy;
	}

	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		// TODO Auto-generated method stub

	}

	@Override
	public void setBeanName(String beanName) {
		// TODO Auto-generated method stub

	}

	@Override
	public void absquatulate() {
		// TODO Auto-generated method stub
		
	}
	
	@Override
	public ITestBean getSpouse() {
		return (spouses != null ? spouses[0] : null);
	}
	@Override
	public void setSpouse(ITestBean spouse) {
		this.spouses = new ITestBean[] {spouse};
	}
	
	@Override
	public ITestBean[] getSpouses() {
		return spouses;
	}
	
	@Override
	public String[] getStringArray() {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
	public void setStringArray(String[] stringArray) {
		// TODO Auto-generated method stub
		
	}
	@Override
	public void exceptional(Throwable t) throws Throwable {
		// TODO Auto-generated method stub
		
	}
	@Override
	public Object returnsThis() {
		// TODO Auto-generated method stub
		return null;
	}
	@Override
	public int haveBirthday() {
		// TODO Auto-generated method stub
		return 0;
	}
	
	@Override
	public INestedTestBean getDoctor() {
		return doctor;
	}
	
	@Override
	public INestedTestBean getLawyer() {
		return lawyer;
	}
	
	public void setDoctor(INestedTestBean doctor) {
		this.doctor = doctor;
	}
	
	public void setLawyer(INestedTestBean lawyer) {
		this.lawyer = lawyer;
	}
	
	@Override
	public IndexedTestBean getNestedIndexedBean() {
		// TODO Auto-generated method stub
		return null;
	}
	@Override
	public void unreliableFileOperation() throws IOException {
		// TODO Auto-generated method stub
		
	}
	
	@Override
	public int hashCode() {
		return age;
	}
	
	@Override
	public boolean equals(Object other) {
		if (this == other) {
			return true;
		}
		if (other == null || !(other instanceof TestBean)) {
			return false;
		}
		TestBean tb2 = (TestBean) other;
		return (ObjectUtils.nullSafeEquals(name, tb2.name) && age == tb2.age);
	}
	
	@Override
	public String toString() {
		return name;
	}

	@Override
	public int compareTo(Object other) {
		if (name != null && other instanceof TestBean) {
			return name.compareTo(((TestBean) other).getName());
		}
		else {
			return 1;
		}
	}
	
}
