package test.beans;

import org.yuan.study.spring.beans.BeansException;
import org.yuan.study.spring.beans.factory.BeanFactory;
import org.yuan.study.spring.beans.factory.BeanFactoryAware;
import org.yuan.study.spring.beans.factory.BeanNameAware;

public class TestBean implements BeanNameAware, BeanFactoryAware, IOther , Comparable {

	private String name;
	
	private int age;
	
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

	@Override
	public int compareTo(Object o) {
		// TODO Auto-generated method stub
		return 0;
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

}
