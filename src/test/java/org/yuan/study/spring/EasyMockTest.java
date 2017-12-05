package org.yuan.study.spring;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletRequest;

import org.easymock.MockControl;
import org.junit.Assert;
import org.junit.Test;

public class EasyMockTest {

	@Test
	public void testLoginFailed() {
		MockControl mc = MockControl.createControl(ServletRequest.class);
		ServletRequest req = (ServletRequest)mc.getMock();
		
		req.getParameter("username");
		mc.setReturnValue("admin", 1);
		req.getParameter("password");
		mc.setReturnValue("1234", 1);
		
		mc.replay();
		
		LoginServlet servlet = new LoginServlet();
		try {
			servlet.service(req, null);
			Assert.fail();
		} 
		catch (Exception  ex) {
			Assert.assertEquals("Login failed", ex.getMessage());
		}
		
		mc.verify();
	}
	
	@Test
	public void testLoginPassed() throws Exception {
		MockControl reqCtrl = MockControl.createControl(ServletRequest.class);
		ServletRequest reqMock = (ServletRequest)reqCtrl.getMock();
		MockControl disCtrl = MockControl.createControl(RequestDispatcher.class);
		RequestDispatcher disMock = (RequestDispatcher)disCtrl.getMock();
		
		reqMock.getParameter("username");
		reqCtrl.setReturnValue("admin", 1);
		reqMock.getParameter("password");
		reqCtrl.setReturnValue("123456", 1);
		reqMock.getRequestDispatcher("main");
		reqCtrl.setReturnValue(disMock);
		disMock.forward(reqMock, null);
		disCtrl.setVoidCallable();
		
		reqCtrl.replay();
		disCtrl.replay();
		
		LoginServlet servlet = new LoginServlet();
		servlet.service(reqMock, null);
		
		reqCtrl.verify();
		disCtrl.verify();
	}
}

