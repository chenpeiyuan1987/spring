package test.beans;

import java.io.IOException;

public interface ITestBean {

	int getAge();
	
	void setAge(int age);
	
	String getName();
	
	void setName(String name);
	
	ITestBean getSpouse();
	
	void setSpouse(ITestBean spouse);
	
	ITestBean[] getSpouses();
	
	String[] getStringArray();
	
	void setStringArray(String[] stringArray);
	
	void exceptional(Throwable t) throws Throwable;
	
	Object returnsThis();
	
	int haveBirthday();
	
	INestedTestBean getDoctor();
	
	INestedTestBean getLawyer();
	
	IndexedTestBean getNestedIndexedBean();
	
	void unreliableFileOperation() throws IOException;
}
