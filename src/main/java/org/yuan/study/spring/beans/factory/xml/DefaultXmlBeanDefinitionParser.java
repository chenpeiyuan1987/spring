package org.yuan.study.spring.beans.factory.xml;

import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.yuan.study.spring.beans.MutablePropertyValues;
import org.yuan.study.spring.beans.factory.BeanDefinitionStoreException;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.beans.factory.config.BeanDefinitionHolder;
import org.yuan.study.spring.beans.factory.config.ConstructorArgumentValues;
import org.yuan.study.spring.beans.factory.support.AbstractBeanDefinition;
import org.yuan.study.spring.beans.factory.support.BeanDefinitionReader;
import org.yuan.study.spring.beans.factory.support.ManagedList;
import org.yuan.study.spring.beans.factory.support.ManagedSet;
import org.yuan.study.spring.beans.factory.support.MethodOverrides;
import org.yuan.study.spring.core.io.Resource;
import org.yuan.study.spring.util.StringUtils;
import org.yuan.study.spring.util.xml.DomUtils;

public class DefaultXmlBeanDefinitionParser implements XmlBeanDefinitionParser {
	
	//------------------------------------------------------------------------
	// 
	//------------------------------------------------------------------------
	public static final String AUTOWIRE_BY_NAME_VALUE = "byName";
	public static final String AUTOWIRE_BY_TYPE_VALUE = "byType";
	public static final String AUTOWIRE_CONSTRUCTOR_VALUE = "constructor";
	public static final String AUTOWIRE_AUTODETECT_VALUE = "autodetect";
	
	public static final String DEPENDENCY_CHECK_ALL_ATTRIBUTE_VALUE = "all";
	public static final String DEPENDENCY_CHECK_SIMPLE_ATTRIBUTE_VALUE = "simple";
	public static final String DEPENDENCY_CHECK_OBJECTS_ATTRIBUTE_VALUE = "objects";
	
	public static final String ALIAS_ELEMENT = "alias";
	public static final String NAME_ATTRIBUTE = "name";
	public static final String ALIAS_ATTRIBUTE = "alias";
	
	public static final String BEAN_ELEMENT = "bean";
	public static final String ID_ATTRIBUTE = "id";
	public static final String PARENT_ATTRIBUTE = "parent";
	
	public static final String CONSTRUCTOR_ARG_ELEMENT = "constructor-arg";
	public static final String PROPERTY_ELEMENT = "property";
	public static final String LOOKUP_METHOD_ELEMENT = "lookup-method";
	public static final String INDEX_ATTRIBUTE = "index";
	public static final String TYPE_ATTRIBUTE = "type";
	public static final String REF_ATTRIBUTE = "ref";
	public static final String VALUE_ATTRIBUTE = "value";
	
	public static final String PROP_ELEMENT = "prop";
	public static final String PROPS_ELEMENT = "props";
	public static final String KEY_ELEMENT = "key";
	public static final String KEY_ATTRIBUTE = "key";
	public static final String KEY_REF_ATTRIBUTE = "key-ref";
	public static final String VALUE_ELEMENT = "value";
	public static final String VALUE_REF_ATTRIBUTE = "value-ref";
	public static final String ENTRY_ELEMENT = "entry";
	public static final String NULL_ELEMENT = "null";
	public static final String LIST_ELEMENT = "list";
	public static final String SET_ELEMENT = "set";
	public static final String MAP_ELEMENT = "map";
	
	
	protected final Log logger = LogFactory.getLog(getClass());
	
	private BeanDefinitionReader beanDefinitionReader;
	
	private Resource resource;
	
	private String defaultLazyInit;
	
	private String defaultAutowire;
	
	private String defaultDependencyCheck;
	
	private String defaultInitMethod;
	
	private String defaultDestroyMethod;
	
	

	//--------------------------------------------------------------------------
	// Implementation of methods
	//--------------------------------------------------------------------------
	
	protected final BeanDefinitionReader getBeanDefinitionReader() {
		return beanDefinitionReader;
	}
	
	protected final Resource getResource() {
		return resource;
	}
	
	protected void initDefaults(Element root) {
		
	}
	
	protected final String getDefaultLazyInit() {
		return defaultLazyInit;
	}

	protected final void setDefaultLazyInit(String defaultLazyInit) {
		this.defaultLazyInit = defaultLazyInit;
	}

	protected final String getDefaultAutowire() {
		return defaultAutowire;
	}

	protected final void setDefaultAutowire(String defaultAutowire) {
		this.defaultAutowire = defaultAutowire;
	}

	protected final String getDefaultDependencyCheck() {
		return defaultDependencyCheck;
	}

	protected final void setDefaultDependencyCheck(String defaultDependencyCheck) {
		this.defaultDependencyCheck = defaultDependencyCheck;
	}

	protected final String getDefaultInitMethod() {
		return defaultInitMethod;
	}

	protected final void setDefaultInitMethod(String defaultInitMethod) {
		this.defaultInitMethod = defaultInitMethod;
	}

	protected final String getDefaultDestroyMethod() {
		return defaultDestroyMethod;
	}

	protected final void setDefaultDestroyMethod(String defaultDestroyMethod) {
		this.defaultDestroyMethod = defaultDestroyMethod;
	}
	
	protected void preProcessXml(Element root) throws BeanDefinitionStoreException {
		
	}
	
	protected int parseBeanDefinition(Element root) throws BeanDefinitionStoreException {
		
	}
	
	protected void importBeanDefinitionResource(Element ele) throws BeanDefinitionStoreException {
		
	}
	
	protected void postProcessXml(Element root) throws BeanDefinitionStoreException {
		
	}
	
	protected BeanDefinitionHolder parseBeanDefinitionElement(Element ele, boolean isInnerBean) throws BeanDefinitionStoreException {
		
	}
	
	protected BeanDefinition parseDeanDefinitionElement(Element ele, String beanName) throws BeanDefinitionStoreException {
		
	}
	
	protected int getDependencyCheck(String attr) {
		int dependencyCheckCode = AbstractBeanDefinition.DEPENDENCY_CHECK_NONE;
		if (DEPENDENCY_CHECK_ALL_ATTRIBUTE_VALUE.equals(attr)) {
			dependencyCheckCode = AbstractBeanDefinition.DEPENDENCY_CHECK_ALL;
		}
		else if (DEPENDENCY_CHECK_SIMPLE_ATTRIBUTE_VALUE.equals(attr)) {
			dependencyCheckCode = AbstractBeanDefinition.DEPENDENCY_CHECK_SIMPLE;
		}
		else if (DEPENDENCY_CHECK_OBJECTS_ATTRIBUTE_VALUE.equals(attr)) {
			dependencyCheckCode = AbstractBeanDefinition.DEPENDENCY_CHECK_OBJECTS;
		}
		
		return dependencyCheckCode;
	}
	
	protected int getAutowireMode(String attr) {
		int autowire = AbstractBeanDefinition.AUTOWIRE_NO;
		if (AUTOWIRE_BY_NAME_VALUE.equals(attr)) {
			autowire = AbstractBeanDefinition.AUTOWIRE_BY_NAME;
		} 
		else if (AUTOWIRE_BY_TYPE_VALUE.equals(attr)) {
			autowire = AbstractBeanDefinition.AUTOWIRE_BY_TYPE;
		}
		else if (AUTOWIRE_CONSTRUCTOR_VALUE.equals(attr)) {
			autowire = AbstractBeanDefinition.AUTOWIRE_CONSTRUCTOR;
		}
		else if (AUTOWIRE_AUTODETECT_VALUE.equals(attr)) {
			autowire = AbstractBeanDefinition.AUTOWIRE_AUTODETECT;
		}
		
		return autowire;
	}
	
	/**
	 * Parse constructor-arg sub-elements of the given bean element.
	 * @param beanEle
	 * @param beanName
	 * @return
	 * @throws BeanDefinitionStoreException
	 */
	protected ConstructorArgumentValues parseConstructorArgElements(Element beanEle, String beanName) throws BeanDefinitionStoreException {
		NodeList nodeList = beanEle.getChildNodes();
		ConstructorArgumentValues cargs = new ConstructorArgumentValues();
		for (int i = 0; i < nodeList.getLength(); i++) {
			Node node = nodeList.item(i);
			if (node instanceof Element && CONSTRUCTOR_ARG_ELEMENT.equals(node.getNodeName())) {
				parseConstructorArgElement((Element)node, beanName, cargs);
			}
		}
		return cargs;
	}
	
	/**
	 * Parse property sub-elements of the given bean element.
	 * @param beanEle
	 * @param beanName
	 * @return
	 * @throws BeanDefinitionStoreException
	 */
	protected MutablePropertyValues parsePropertyElements(Element beanEle, String beanName) throws BeanDefinitionStoreException {
		NodeList nodeList = beanEle.getChildNodes();
		MutablePropertyValues pvs = new MutablePropertyValues();
		for (int i = 0; i < nodeList.getLength(); i++) {
			Node node = nodeList.item(i);
			if (node instanceof Element && PROPERTY_ELEMENT.equals(node.getNodeName())) {
				parsePropertyElement((Element)node, beanName, pvs);
			}
		}
		return pvs;
	}
	
	protected void parseLookupOverrideSubElements(Element beanEle, String beanName, MethodOverrides overrides) throws BeanDefinitionStoreException {
		
	}
	
	protected void parseReplacedMethodSubElements(Element beanEle, String beanName, MethodOverrides overrides) throws BeanDefinitionStoreException {
		
	}
	
	protected void parseConstructorArgElement(Element ele, String beanName, ConstructorArgumentValues cargs) throws BeanDefinitionStoreException {
		
	}
	
	/**
	 * Parse a property element.
	 * @param ele
	 * @param beanName
	 * @param pvs
	 * @throws BeanDefinitionStoreException
	 */
	protected void parsePropertyElement(Element ele, String beanName, MutablePropertyValues pvs) throws BeanDefinitionStoreException {
		String propertyName = ele.getAttribute(NAME_ATTRIBUTE);
		if (!StringUtils.hasLength(propertyName)) {
			throw new BeanDefinitionStoreException(
				getResource(), beanName, "Tag 'property' must have a 'name' attribute");
		}
		if (pvs.contains(propertyName)) {
			throw new BeanDefinitionStoreException(
				getResource(), beanName, String.format("Multiple 'property' definitions for property '%s'", propertyName));
		}
		Object val = parsePropertyValue(ele, beanName, propertyName);
		pvs.addPropertyValue(propertyName, val);
	}
	
	protected Object parsePropertyValue(Element ele, String beanName, String propertyName) throws BeanDefinitionStoreException {
		
	}
	
	protected Object parsePropertySubElement(Element ele, String beanName) throws BeanDefinitionStoreException {
		
	}
	
	/**
	 * Parse a list element.
	 * @param collectionEle
	 * @param beanName
	 * @return
	 * @throws BeanDefinitionStoreException
	 */
	protected List<Object> parseListElement(Element collectionEle, String beanName) throws BeanDefinitionStoreException {
		NodeList nodeList = collectionEle.getChildNodes();
		ManagedList<Object> list = new ManagedList<Object>(nodeList.getLength());
		for (int i = 0; i < nodeList.getLength(); i++) {
			Node node = nodeList.item(i);
			if (node instanceof Element) {
				Element element = (Element) node;
				list.add(parsePropertySubElement(element, beanName));
			}
		}
		return list;
	}
	
	/**
	 * Parse a set element.
	 * @param collectionEle
	 * @param beanName
	 * @return
	 * @throws BeanDefinitionStoreException
	 */
	protected Set<Object> parseSetElement(Element collectionEle, String beanName) throws BeanDefinitionStoreException {
		NodeList nodeList = collectionEle.getChildNodes();
		ManagedSet<Object> set = new ManagedSet<Object>(nodeList.getLength());
		for (int i = 0; i < nodeList.getLength(); i++) {
			Node node = nodeList.item(i);
			if (node instanceof Element) {
				Element element = (Element) node;
				set.add(parsePropertySubElement(element, beanName));
			}
		}
		return set;
	}
	
	protected Map parseMapElement(Element mapEle, String beanName) throws BeanDefinitionStoreException {
		
	}
	
	/**
	 * Parse a key sub-element of a map element.
	 * @param keyEle
	 * @param beanName
	 * @return
	 * @throws BeanDefinitionStoreException
	 */
	protected Object parseKeyElement(Element keyEle, String beanName) throws BeanDefinitionStoreException {
		NodeList nodeList = keyEle.getChildNodes();
		Element subElement = null;
		for (int i = 0; i < nodeList.getLength(); i++) {
			Node node = nodeList.item(i);
			if (node instanceof Element) {
				Element element = (Element) node;
				if (subElement != null) {
					throw new BeanDefinitionStoreException(
						getResource(), beanName, "<key> element must not contain more than one value sub-element");
				}
				subElement = element;
			}
		}
		return parsePropertySubElement(subElement, beanName);
	}
	
	/**
	 * Parse a props element.
	 * @param propsEle
	 * @param beanName
	 * @return
	 * @throws BeanDefinitionStoreException
	 */
	protected Properties parsePropsElement(Element propsEle, String beanName) throws BeanDefinitionStoreException {
		Properties props = new Properties();
		List<Element> propEles = DomUtils.getChildElementsByTagName(propsEle, PROP_ELEMENT);
		for (Element propEle : propEles) {
			String key = propEle.getAttribute(KEY_ATTRIBUTE);
			String val = DomUtils.getTextValue(propEle).trim();
			props.setProperty(key, val);
		}
		return props;
	}
	
	
	//--------------------------------------------------------------------------
	// Implementation of XmlBeanDefinitionParser interface
	//--------------------------------------------------------------------------
	
	

	@Override
	public int registerBeanDefinitions(BeanDefinitionReader reader, Document doc, Resource resource) {
		// TODO Auto-generated method stub
		return 0;
	}

}
