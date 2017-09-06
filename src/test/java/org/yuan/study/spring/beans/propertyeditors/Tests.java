package org.yuan.study.spring.beans.propertyeditors;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses({
	BeanInfoTest.class,
	ByteArrayPropertyEditorTest.class,
	CharArrayPropertyEditorTest.class,
	CustomCollectionEditorTest.class,
})
public class Tests {

}
