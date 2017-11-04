package org.yuan.study.spring.util;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses({
	AssertTest.class,
	ClassUtilsTest.class,
	StringUtilsTest.class,
	SystemPropertyUtilsTest.class,
	PropertyPlaceholderHelperTest.class
})
public class Tests {

}
