package org.yuan.study.spring.util;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses({
	AssertTest.class,
	ClassUtilsTest.class,
	ObjectUtilsTest.class,
	StringUtilsTest.class,
	NumberUtilsTest.class,
	CollectionUtilsTest.class,
	SystemPropertyUtilsTest.class,
	PropertyPlaceholderHelperTest.class
})
public class Tests {

}
