package org.yuan.study.spring.util;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses({
	AntPathMatcherTest.class,
	AssertTest.class,
	ClassUtilsTest.class,
	CollectionUtilsTest.class,
	LinkedMultiValueMapTest.class,
	NumberUtilsTest.class,
	ObjectUtilsTest.class,
	PropertyPlaceholderHelperTest.class,
	ReflectionUtilsTest.class,
	ResourceUtilsTest.class,
	StringUtilsTest.class,
	SystemPropertyUtilsTest.class,
})
public class Tests {

}
