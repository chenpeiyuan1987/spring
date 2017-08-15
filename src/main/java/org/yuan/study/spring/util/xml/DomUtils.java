package org.yuan.study.spring.util.xml;

import java.util.ArrayList;
import java.util.List;

import org.w3c.dom.CharacterData;
import org.w3c.dom.Comment;
import org.w3c.dom.Element;
import org.w3c.dom.EntityReference;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.yuan.study.spring.util.Assert;

public abstract class DomUtils {

	/**
	 * Retrieve all child elements of the given DOM element that match the given element name.
	 * Only look at the direct child level of the given element; do not go into further depth.
	 * @param ele
	 * @param childEleName
	 * @return
	 */
	public static List<Element> getChildElementsByTagName(Element ele, String childEleName) {
		NodeList nodeList = ele.getChildNodes();
		List<Element> childEles = new ArrayList<Element>();
		for (int i=0; i<nodeList.getLength(); i++) {
			Node node = nodeList.item(i);
			if (node instanceof Element && nodeNameEquals(node, childEleName)) {
				childEles.add((Element) node);
			}
		}
		return childEles;
	}
	
	/**
	 * Namespace-aware equals comparison. 
	 * Returns true if either or equals desiredName otherwise returns false.
	 * @param node
	 * @param desiredName
	 * @return
	 */
	public static boolean nodeNameEquals(Node node, String desiredName) {
		Assert.notNull(node, "Node must not be null");
		Assert.notNull(desiredName, "Desired name must not be null");
		return desiredName.equals(node.getNodeName()) || desiredName.equals(node.getLocalName());
	}
	
	/**
	 * Extract the text value from the given DOM element, ignoring XML comments.
	 * Appends all CharacterData nodes and EntityReference nodes into a single
	 * String value, excluding Comment nodes.
	 * @param valueEle
	 * @return
	 */
	public static String getTextValue(Element valueEle) {
		StringBuffer value = new StringBuffer();
		NodeList nodeList = valueEle.getChildNodes();
		for (int i = 0; i < nodeList.getLength(); i++) {
			Node node = nodeList.item(i);
			if ((node instanceof CharacterData && !(node instanceof Comment)) 
				|| node instanceof EntityReference) {
				value.append(node.getNodeValue());
			}
		}
		return value.toString();
	}
	
	/**
	 * Return the first child element identified by this name.
	 * @param ele
	 * @param childEleName
	 * @return
	 */
	public static Element getChildElementByTagName(Element ele, String childEleName) {
		NodeList nodeList = ele.getChildNodes();
		for (int i=0; i<nodeList.getLength(); i++) {
			Node node = nodeList.item(i);
			if (node instanceof Element && nodeNameEquals(node, childEleName)) {
				return (Element) node;
			}
		}
		return null;
	}
	
	/**
	 * Return the first child element value identified by its name.
	 * @param ele
	 * @param childEleName
	 * @return
	 */
	public static String getChildElementValueByTagName(Element ele, String childEleName) {
		Element child = getChildElementByTagName(ele, childEleName);
		return (child != null ? getTextValue(child) : null);
	}
}
