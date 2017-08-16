package org.yuan.study.spring.beans.factory.xml;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.core.io.support.ResourcePatternUtils;
import org.springframework.util.SystemPropertyUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.yuan.study.spring.beans.MutablePropertyValues;
import org.yuan.study.spring.beans.factory.BeanDefinitionStoreException;
import org.yuan.study.spring.beans.factory.config.BeanDefinition;
import org.yuan.study.spring.beans.factory.config.BeanDefinitionHolder;
import org.yuan.study.spring.beans.factory.config.ConstructorArgumentValues;
import org.yuan.study.spring.beans.factory.config.RuntimeBeanReference;
import org.yuan.study.spring.beans.factory.support.AbstractBeanDefinition;
import org.yuan.study.spring.beans.factory.support.BeanDefinitionReader;
import org.yuan.study.spring.beans.factory.support.BeanDefinitionReaderUtils;
import org.yuan.study.spring.beans.factory.support.LookupOverride;
import org.yuan.study.spring.beans.factory.support.ManagedList;
import org.yuan.study.spring.beans.factory.support.ManagedMap;
import org.yuan.study.spring.beans.factory.support.ManagedSet;
import org.yuan.study.spring.beans.factory.support.MethodOverrides;
import org.yuan.study.spring.beans.factory.support.ReplaceOverride;
import org.yuan.study.spring.core.io.Resource;
import org.yuan.study.spring.util.StringUtils;
import org.yuan.study.spring.util.xml.DomUtils;

public class DefaultXmlBeanDefinitionParser implements XmlBeanDefinitionParser {
	
	//------------------------------------------------------------------------
	// 
	//------------------------------------------------------------------------
	public static final String BEAN_NAME_DELIMITERS = ",; ";
	
	public static final String TRUE_VALUE = "true";
	public static final String DEFAULT_VALUE = "default";
	public static final String DESCRIPTION_ELEMENT= "description";
	
	public static final String AUTOWIRE_BY_NAME_VALUE = "byName";
	public static final String AUTOWIRE_BY_TYPE_VALUE = "byType";
	public static final String AUTOWIRE_CONSTRUCTOR_VALUE = "constructor";
	public static final String AUTOWIRE_AUTODETECT_VALUE = "autodetect";
	
	public static final String DEPENDENCY_CHECK_ALL_ATTRIBUTE_VALUE = "all";
	public static final String DEPENDENCY_CHECK_SIMPLE_ATTRIBUTE_VALUE = "simple";
	public static final String DEPENDENCY_CHECK_OBJECTS_ATTRIBUTE_VALUE = "objects";
	
	public static final String DEFAULT_LAZY_INIT_ATTRIBUTE = "default-lazy-init";
	public static final String DEFAULT_AUTOWIRE_ATTRIBUTE = "default-autowire";
	public static final String DEFAULT_DEPENDENCY_CHECK_ATTRIBUTE = "default-dependency-check";
	public static final String DEFAULT_INIT_METHOD_ATTRIBUTE = "default-init-method";
	public static final String DEFAULT_DESTROY_METHOD_ATTRIBUTE = "default-destroy-method";
	
	public static final String IMPORT_ELEMENT = "import";
	public static final String RESOURCE_ATTRIBUTE = "resource";
	
	public static final String ALIAS_ELEMENT = "alias";
	public static final String NAME_ATTRIBUTE = "name";
	public static final String ALIAS_ATTRIBUTE = "alias";
	
	public static final String BEAN_ELEMENT = "bean";
	public static final String ID_ATTRIBUTE = "id";
	public static final String PARENT_ATTRIBUTE = "parent";
	
	public static final String CLASS_ATTRIBUTE = "class";
	public static final String ABSTRACT_ATTRIBUTE = "abstract";
	public static final String SINGLETON_ATTRIBUTE = "singleton";
	public static final String LAZY_INIT_ATTRIBUTE = "lazy-init";
	public static final String AUTOWIRE_ATTRIBUTE = "autowire";
	public static final String DEPENDENCY_CHECK_ATTRIBUTE = "dependency-check";
	public static final String DEPENDS_ON_ATTRIBUTE = "depends-on";
	public static final String INIT_METHOD_ATTRIBUTE = "init-method";
	public static final String DESTROY_METHOD_ATTRIBUTE = "destroy-method";
	public static final String FACTORY_METHOD_ATTRIBUTE = "factory-method";
	public static final String FACTORY_BEAN_ATTRIBUTE = "factory-bean";
	
	public static final String CONSTRUCTOR_ARG_ELEMENT = "constructor-arg";
	public static final String PROPERTY_ELEMENT = "property";
	public static final String LOOKUP_METHOD_ELEMENT = "lookup-method";
	public static final String INDEX_ATTRIBUTE = "index";
	public static final String TYPE_ATTRIBUTE = "type";
	public static final String REF_ATTRIBUTE = "ref";
	public static final String VALUE_ATTRIBUTE = "value";
	
	public static final String REPLACED_METHOD_ELEMENT = "replaced-method";
	public static final String REPLACER_ATTRIBUTE = "replacer";
	public static final String ARG_TYPE_ELEMENT = "arg-type";
	public static final String ARG_TYPE_MATCH_ATTRIBUTE = "match";
	
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
	
	/**
	 * Initialize the default lazy-init, autowire and dependency check settings.
	 * @param root
	 */
	protected void initDefaults(Element root) {
		setDefaultLazyInit(root.getAttribute(DEFAULT_LAZY_INIT_ATTRIBUTE));
		setDefaultAutowire(root.getAttribute(DEFAULT_AUTOWIRE_ATTRIBUTE));
		setDefaultDependencyCheck(root.getAttribute(DEFAULT_DEPENDENCY_CHECK_ATTRIBUTE));
		if (root.hasAttribute(DEFAULT_INIT_METHOD_ATTRIBUTE)) {
			setDefaultInitMethod(root.getAttribute(DEFAULT_INIT_METHOD_ATTRIBUTE));
		}
		if (root.hasAttribute(DEFAULT_DESTROY_METHOD_ATTRIBUTE)) {
			setDefaultDestroyMethod(root.getAttribute(DEFAULT_DESTROY_METHOD_ATTRIBUTE));
		}
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
	
	/**
	 * 
	 * @param root
	 * @throws BeanDefinitionStoreException
	 */
	protected void preProcessXml(Element root) throws BeanDefinitionStoreException {
		
	}
	
	/**
	 * Parse the elements at the root level in the document:
	 * "import", "alias", "bean"
	 * @param root
	 * @return
	 * @throws BeanDefinitionStoreException
	 */
	protected int parseBeanDefinitions(Element root) throws BeanDefinitionStoreException {
		NodeList nodeList = root.getChildNodes();
		int beanDefinitionCount = 0;
		for (int i = 0; i < nodeList.getLength(); i++) {
			Node node = nodeList.item(i);
			if (node instanceof Element) {
				Element ele = (Element) node;
				if (IMPORT_ELEMENT.equals(node.getNodeName())) {
					importBeanDefinitionResource(ele);
				}
				else if (ALIAS_ELEMENT.equals(node.getNodeName())) {
					String name = ele.getAttribute(NAME_ATTRIBUTE);
					String alias = ele.getAttribute(ALIAS_ATTRIBUTE);
					this.beanDefinitionReader.getBeanFactory().registerAlias(name, alias);
				}
				else if (BEAN_ELEMENT.equals(node.getNodeName())) {
					beanDefinitionCount++;
					BeanDefinitionHolder holder = parseBeanDefinitionElement(ele, false);
					BeanDefinitionReaderUtils.registerBeanDefinition(holder, this.beanDefinitionReader.getBeanFactory());
				}
			}
		}
		return beanDefinitionCount;
	}
	
	/**
	 * Parse an "import" element and load the beran definitions 
	 * from the given resource into the bean factory.
	 * @param ele
	 * @throws BeanDefinitionStoreException
	 */
	protected void importBeanDefinitionResource(Element ele) throws BeanDefinitionStoreException {
		String location = ele.getAttribute(RESOURCE_ATTRIBUTE);
		location = SystemPropertyUtils.resolvePlaceholders(location);
		
		if (ResourcePatternUtils.isUrl(location)) {
			int importCount = getBeanDefinitionReader().loadBeanDefinitions(location);
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("Imported %s bean definitions from URL location [%s]", importCount, location));
			}
		}
		else {
			try {
				Resource relativeResource = getResource().createRelative(location);
				int importCount = getBeanDefinitionReader().loadBeanDefinitions(relativeResource);
				if (logger.isDebugEnabled()) {
					logger.debug(String.format("Imported %s bean definitions from relative location [%s]", importCount, location));
				}
			}
			catch (IOException ex) {
				throw new BeanDefinitionStoreException(
					String.format("Invalid relative resource location [%s] to import bean definitions from", location), ex);
			}
		}
	}
	
	/**
	 * 
	 * @param root
	 * @throws BeanDefinitionStoreException
	 */
	protected void postProcessXml(Element root) throws BeanDefinitionStoreException {
		
	}
	
	/**
	 * Parse a standard bean definition into a BeanDefinitionHolder,
	 * including bean name and aliases.
	 * @param ele
	 * @param isInnerBean
	 * @return
	 * @throws BeanDefinitionStoreException
	 */
	protected BeanDefinitionHolder parseBeanDefinitionElement(Element ele, boolean isInnerBean) throws BeanDefinitionStoreException {
		String id = ele.getAttribute(ID_ATTRIBUTE);
		String nameAttr = ele.getAttribute(NAME_ATTRIBUTE);
		
		List<String> aliases = new ArrayList<String>();
		if (StringUtils.hasLength(nameAttr)) {
			String[] nameArr = StringUtils.tokenizeToStringArray(nameAttr, BEAN_NAME_DELIMITERS);
			aliases.addAll(Arrays.asList(nameArr));
		}
		
		String beanName = id;
		if (!StringUtils.hasText(beanName) && !aliases.isEmpty()) {
			beanName = (String) aliases.remove(0);
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("No XML 'id' specified - using '%s' as bean name and %s as aliases", beanName, aliases));
			}
		}
		
		BeanDefinition beanDefinition = parseBeanDefinitionElement(ele, beanName);
		if (!StringUtils.hasText(beanName) && beanDefinition instanceof AbstractBeanDefinition) {
			beanName = BeanDefinitionReaderUtils.generateBeanName((AbstractBeanDefinition) beanDefinition, this.beanDefinitionReader.getBeanFactory(), isInnerBean);
			if (logger.isDebugEnabled()) {
				logger.debug(String.format("Neither XML 'id' nor 'name' specified - using generated bean name [%s]", beanName));
			}
		}
		
		String[] aliasesArray = StringUtils.toStringArray(aliases);
		return new BeanDefinitionHolder(beanDefinition, beanName, aliasesArray);
	}
	
	/**
	 * Parse the BeanDefinition itself, without regard to name or aliases.
	 * @param ele
	 * @param beanName
	 * @return
	 * @throws BeanDefinitionStoreException
	 */
	protected BeanDefinition parseBeanDefinitionElement(Element ele, String beanName) throws BeanDefinitionStoreException {
		String className = null;
		if (ele.hasAttribute(CLASS_ATTRIBUTE)) {
			className = ele.getAttribute(CLASS_ATTRIBUTE).trim();
		}
		String parent = null;
		if (ele.hasAttribute(PARENT_ATTRIBUTE)) {
			parent = ele.getAttribute(PARENT_ATTRIBUTE);
		}
		
		try {
			ConstructorArgumentValues cargs = parseConstructorArgElements(ele, beanName);
			MutablePropertyValues pvs = parsePropertyElements(ele, beanName);
			
			AbstractBeanDefinition bd = BeanDefinitionReaderUtils.createBeanDefinition(
				className, parent, cargs, pvs, getBeanDefinitionReader().getBeanClassLoader());
			
			if (ele.hasAttribute(DEPENDS_ON_ATTRIBUTE)) {
				String dependsOn = ele.getAttribute(DEPENDS_ON_ATTRIBUTE);
				bd.setDependsOn(StringUtils.tokenizeToStringArray(dependsOn, BEAN_NAME_DELIMITERS));
			}
			
			if (ele.hasAttribute(FACTORY_METHOD_ATTRIBUTE)) {
				bd.setFactoryMethodName(ele.getAttribute(FACTORY_METHOD_ATTRIBUTE));
			}
			if (ele.hasAttribute(FACTORY_BEAN_ATTRIBUTE)) {
				bd.setFactoryBeanName(ele.getAttribute(FACTORY_BEAN_ATTRIBUTE));
			}
			
			String dependencyCheck = ele.getAttribute(DEPENDENCY_CHECK_ATTRIBUTE);
			if (DEFAULT_VALUE.equals(dependencyCheck)) {
				dependencyCheck = getDefaultDependencyCheck();
			}
			bd.setDependencyCheck(getDependencyCheck(dependencyCheck));
			
			String autowire = ele.getAttribute(AUTOWIRE_ATTRIBUTE);
			if (DEFAULT_VALUE.equals(autowire)) {
				autowire = getDefaultAutowire();
			}
			bd.setAutowireMode(getAutowireMode(autowire));
			
			if (ele.hasAttribute(INIT_METHOD_ATTRIBUTE)) {
				String initMethodName = ele.getAttribute(INIT_METHOD_ATTRIBUTE);
				if (!"".endsWith(initMethodName)) {
					bd.setInitMethodName(initMethodName);
				}
			} 
			else {
				if (getDefaultInitMethod() != null) {
					bd.setInitMethodName(getDefaultInitMethod());
					bd.setEnforceInitMethod(false);
				}
			}
			
			if (ele.hasAttribute(DESTROY_METHOD_ATTRIBUTE)) {
				String destroyMethodName = ele.getAttribute(DESTROY_METHOD_ATTRIBUTE);
				if (!"".equals(destroyMethodName)) {
					bd.setDestroyMethodName(destroyMethodName);
				}
			} 
			else {
				if (getDefaultDestroyMethod() != null) {
					bd.setDestroyMethodName(getDefaultDestroyMethod());
					bd.setEnforceDestroyMethod(false);
				}
			}
			
			parseLookupOverrideSubElements(ele, beanName, bd.getMethodOverrides());
			parseReplacedMethodSubElements(ele, beanName, bd.getMethodOverrides());
			
			bd.setResourceDescription(getResource().getDescription());
			
			if (ele.hasAttribute(ABSTRACT_ATTRIBUTE)) {
				bd.setAbstract(TRUE_VALUE.equals(ele.getAttribute(ABSTRACT_ATTRIBUTE)));
			}
			
			if (ele.hasAttribute(SINGLETON_ATTRIBUTE)) {
				bd.setSingleton(TRUE_VALUE.equals(ele.getAttribute(SINGLETON_ATTRIBUTE)));
			}
			
			String lazyInit = ele.getAttribute(LAZY_INIT_ATTRIBUTE);
			if (DEFAULT_VALUE.equals(lazyInit) && bd.isSingleton()) {
				lazyInit = getDefaultLazyInit();
			}
			bd.setLazyInit(TRUE_VALUE.equals(lazyInit));
			
			return bd;
		}
		catch (BeanDefinitionStoreException ex) {
			throw ex;
		}
		catch (ClassNotFoundException ex) {
			throw new BeanDefinitionStoreException(
				getResource(), beanName, String.format("Bean class [%s] not found", className), ex);
		}
		catch (NoClassDefFoundError ex) {
			throw new BeanDefinitionStoreException(
				getResource(), beanName, String.format("Class that bean class [%s] depends on not found", className), ex);
		}
		catch (Throwable ex) {
			throw new BeanDefinitionStoreException(
				getResource(), beanName, "Unexpected failure during bean definition parsing", ex);
		}
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
	
	/**
	 * Parse look-override sub-elements of the given bean element.
	 * @param beanEle
	 * @param beanName
	 * @param overrides
	 * @throws BeanDefinitionStoreException
	 */
	protected void parseLookupOverrideSubElements(Element beanEle, String beanName, MethodOverrides overrides) 
		throws BeanDefinitionStoreException {
		NodeList nodeList = beanEle.getChildNodes();
		for (int i = 0; i < nodeList.getLength(); i++) {
			Node node = nodeList.item(i);
			if (node instanceof Element && LOOKUP_METHOD_ELEMENT.equals(node.getNodeName())) {
				Element ele = (Element) node;
				String methodName = ele.getAttribute(NAME_ATTRIBUTE);
				String beanRef = ele.getAttribute(BEAN_ELEMENT);
				overrides.addOverride(new LookupOverride(methodName, beanRef));
			}
		}
	}
	
	/**
	 * Parse replaced-method sub-elements of the given bean element.
	 * @param beanEle
	 * @param beanName
	 * @param overrides
	 * @throws BeanDefinitionStoreException
	 */
	protected void parseReplacedMethodSubElements(Element beanEle, String beanName, MethodOverrides overrides) 
		throws BeanDefinitionStoreException {
		NodeList nodeList = beanEle.getChildNodes();
		for (int i = 0; i < nodeList.getLength(); i++) {
			Node node = nodeList.item(i);
			if (node instanceof Element && REPLACED_METHOD_ELEMENT.equals(node.getNodeName())) {
				Element ele = (Element) node;
				String name = ele.getAttribute(NAME_ATTRIBUTE);
				String callback = ele.getAttribute(REPLACER_ATTRIBUTE);
				ReplaceOverride replaceOverride = new ReplaceOverride(name, callback);
				List<Element> argTypeEles = DomUtils.getChildElementsByTagName(ele, ARG_TYPE_ELEMENT);
				for (Element argTypeEle : argTypeEles) {
					replaceOverride.addTypeIdentifier(argTypeEle.getAttribute(ARG_TYPE_MATCH_ATTRIBUTE));
				}
				overrides.addOverride(replaceOverride);
			}
		}
	}
	
	protected void parseConstructorArgElement(Element ele, String beanName, ConstructorArgumentValues cargs) 
		throws BeanDefinitionStoreException {
		Object val = parsePropertyValue(ele, beanName, null);
		String indexAttr = ele.getAttribute(INDEX_ATTRIBUTE);
		String typeAttr = ele.getAttribute(TYPE_ATTRIBUTE);
		if (StringUtils.hasLength(indexAttr)) {
			try {
				int index = Integer.parseInt(indexAttr);
				if (index < 0) {
					throw new BeanDefinitionStoreException(getResource(), beanName, "'index' cannot be lower than 0");
				}
				if (StringUtils.hasLength(typeAttr)) {
					cargs.addIndexedArgumentValue(index, val, typeAttr);
				} 
				else {
					cargs.addIndexedArgumentValue(index, val);
				}
			}
			catch (NumberFormatException ex) {
				throw new BeanDefinitionStoreException(
					getResource(), beanName, "Attribute 'index' of tag 'constructor-arg' must be an integer");
			}
		} else {
			if (StringUtils.hasLength(typeAttr)) {
				cargs.addGenericArgumentValue(val, typeAttr);
			} 
			else {
				cargs.addGenericArgumentValue(val);
			}
		}
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
	
	/**
	 * Parse a map element
	 * @param mapEle
	 * @param beanName
	 * @return
	 * @throws BeanDefinitionStoreException
	 */
	protected Map<Object,Object> parseMapElement(Element mapEle, String beanName) throws BeanDefinitionStoreException {
		List<Element> entryEles = DomUtils.getChildElementsByTagName(mapEle, ENTRY_ELEMENT);
		ManagedMap<Object,Object> map = new ManagedMap<Object,Object>();
		
		for (Element entryEle : entryEles) {
			NodeList nodeList = entryEle.getChildNodes();
			
			Element keyEle = null;
			Element valEle = null;
			for (int i=0; i<nodeList.getLength(); i++) {
				Node node = nodeList.item(i);
				if (node instanceof Element) {
					Element element = (Element) node;
					if (element.getTagName().equals(KEY_ELEMENT)) {
						if (keyEle != null) {
							throw new BeanDefinitionStoreException(
								getResource(), beanName, "<entry> element is only allowed to contain one <key> sub-element");
						}
						keyEle = element;
					} 
					else {
						if (valEle != null) {
							throw new BeanDefinitionStoreException(
								getResource(), beanName, "<entry> element must not contain more than one value sub-element");
						}
						valEle = element;
					}
				}
			}
			
			// Extract key from attribute or sub-element
			Object key = null;
			boolean hasKeyAttribute = entryEle.hasAttribute(KEY_ATTRIBUTE);
			boolean hasKeyRefAttribute = entryEle.hasAttribute(KEY_REF_ATTRIBUTE);
			if ((hasKeyAttribute && hasKeyRefAttribute) || (hasKeyAttribute || hasKeyRefAttribute) && keyEle != null) {
				throw new BeanDefinitionStoreException(getResource(), beanName, 
					"<entry> element is only allowed to contain either a 'key' attribute OR a 'key-ref' attribute OR a <key> sub-element");
			}
			if (hasKeyAttribute) {
				key = entryEle.getAttribute(KEY_ATTRIBUTE);
			}
			else if (hasKeyRefAttribute) {
				String refName = entryEle.getAttribute(KEY_REF_ATTRIBUTE);
				if (!StringUtils.hasText(refName)) {
					throw new BeanDefinitionStoreException(getResource(), beanName, "<entry> element contains empty 'key-ref' attribute");
				}
				key = new RuntimeBeanReference(refName);
			}
			else if (keyEle != null) {
				key = parseKeyElement(keyEle, beanName);
			}
			else {
				throw new BeanDefinitionStoreException(getResource(), beanName, "<entry> element must specify a key");
			}
			
			// Extract val from attribute or sub-element
			Object val = null;
			boolean hasValAttribute = entryEle.hasAttribute(KEY_ATTRIBUTE);
			boolean hasValRefAttribute = entryEle.hasAttribute(KEY_REF_ATTRIBUTE);
			if ((hasValAttribute && hasValRefAttribute) || (hasValAttribute || hasValRefAttribute) && valEle != null) {
				throw new BeanDefinitionStoreException(getResource(), beanName, 
					"<entry> element is only allowed to contain either a 'value' attribute OR a 'value-ref' attribute OR a <value> sub-element");
			}
			if (hasValAttribute) {
				val = entryEle.getAttribute(VALUE_ATTRIBUTE);
			}
			else if (hasValRefAttribute) {
				String refName = entryEle.getAttribute(VALUE_REF_ATTRIBUTE);
				if (!StringUtils.hasText(refName)) {
					throw new BeanDefinitionStoreException(getResource(), beanName, "<entry> element contains empty 'value-ref' attribute");
				}
				val = new RuntimeBeanReference(refName);
			}
			else if (valEle != null) {
				val = parsePropertySubElement(valEle, beanName);
			}
			else {
				throw new BeanDefinitionStoreException(getResource(), beanName, "<entry> element must specify a value");
			}
			
			map.put(key, val);
		}
		
		return map;
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
		this.beanDefinitionReader = reader;
		this.resource = resource;
		
		logger.debug("Loading bean definitions");
		Element root = doc.getDocumentElement();
		
		initDefaults(root);
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Default lazy init '%s'", getDefaultLazyInit()));
			logger.debug(String.format("Default autowire '%s'", getDefaultAutowire()));
			logger.debug(String.format("Default dependency check '%s'", getDefaultDependencyCheck()));
		}
		
		preProcessXml(root);
		int beanDefinitionCount = parseBeanDefinitions(root);
		if (logger.isDebugEnabled()) {
			logger.debug(String.format("Found %s <bean> elements in %s", beanDefinitionCount, resource));
		}
		postProcessXml(root);
		
		return beanDefinitionCount;
	}

}
