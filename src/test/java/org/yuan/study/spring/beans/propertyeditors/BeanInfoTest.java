package org.yuan.study.spring.beans.propertyeditors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.beans.IntrospectionException;
import java.beans.PropertyDescriptor;
import java.beans.SimpleBeanInfo;

import org.junit.Test;
import org.yuan.study.spring.beans.BeanWrapper;
import org.yuan.study.spring.beans.BeanWrapperImpl;
import org.yuan.study.spring.beans.FatalBeanException;
import org.yuan.study.spring.core.JdkVersion;
import org.yuan.study.spring.util.Assert;

public class BeanInfoTest {

	@Test
	public void testComplexObject() {
		ValueBean bean = new ValueBean();
		BeanWrapper bw = new BeanWrapperImpl(bean);
		Integer value  = new Integer(1);
		bw.registerCustomEditor(Integer.class, new MyNumberEditor());
		
		bw.setPropertyValue("value", value);
		assertEquals(value, bean.getValue());
		
		value = new Integer(2);
		bw.setPropertyValue("value", value.toString());
		assertEquals(value, bean.getValue());
		
		bw.setPropertyValue("value", null);
		assertNull(bean.getValue());
		
		bw.setPropertyValue("value", "");
		assertNull(bean.getValue());
	}
	
	//-----------------------------------
	// Class for test
	//-----------------------------------
	
	public static class ValueBean {
		private Integer value;

		public Integer getValue() {
			return value;
		}

		public void setValue(Integer value) {
			this.value = value;
		}
	}
	/*
	public static class ValueBeanBeanInfo extends SimpleBeanInfo {
		@Override
		public PropertyDescriptor[] getPropertyDescriptors() {
			try {
				PropertyDescriptor pd = new PropertyDescriptor("value", ValueBean.class);
				pd.setPropertyEditorClass(MyNumberEditor.class);
				return new PropertyDescriptor[] {pd};
			}
			catch (IntrospectionException ex) {
				throw new FatalBeanException("Couldn't create PropertyDescriptor", ex);
			}
		}
	}
	*/
	public static class MyNumberEditor extends CustomNumberEditor {
		private Object target;
		
		public MyNumberEditor() throws IllegalArgumentException {
			super(Integer.class, true);
		}

		public MyNumberEditor(Object target) throws IllegalArgumentException {
			super(Integer.class, true);
			this.target = target;
		}

		@Override
		public void setAsText(String text) throws IllegalArgumentException {
			if (JdkVersion.getMajorJavaVersion() >= JdkVersion.JAVA_15) {
				Assert.isTrue(this.target instanceof ValueBean);
			}
			super.setAsText(text);
		}
		
	}
	
}
