package org.yuan.study.spring.beans.propertyeditors;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses({
	BeanInfoTest.class,
	URLEditorTest.class,
	FileEditorTest.class,
	PropertiesEditorTest.class,
	InputStreamEditorTest.class,
	CustomCollectionEditorTest.class,
	ByteArrayPropertyEditorTest.class,
	CharArrayPropertyEditorTest.class,
	StringArrayPropertyEditorTest.class,
})
public class Tests {

}
