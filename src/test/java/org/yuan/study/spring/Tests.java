package org.yuan.study.spring;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses({
	org.yuan.study.spring.util.Tests.class,
	org.yuan.study.spring.core.Tests.class,
	org.yuan.study.spring.beans.Tests.class,
})
public class Tests {

}
