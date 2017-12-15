package org.yuan.study.spring.beans;

import org.yuan.study.spring.core.MethodParameter;
import org.yuan.study.spring.core.convert.ConversionException;
import org.yuan.study.spring.core.convert.ConverterNotFoundException;

public class SimpleTypeConverter extends PropertyEditorRegistrySupport implements TypeConverter {

	private final TypeConverterDelegate typeConverterDelegate = new TypeConverterDelegate(this);
	
	public SimpleTypeConverter() {
		registerDefaultEditors();
	}

	@Override
	public <T> T convertIfNecessary(Object value, Class<T> requiredType) throws TypeMismatchException {
		return convertIfNecessary(value, requiredType, null);
	}

	@Override
	public <T> T convertIfNecessary(Object value, Class<T> requiredType, MethodParameter methodParam) throws TypeMismatchException {
		try {
			return typeConverterDelegate.convertIfNecessary(value, requiredType, methodParam);
		} 
		catch (ConverterNotFoundException ex) {
			throw new ConversionNotSupportedException(value, requiredType, ex);
		}
		catch (ConversionException ex) {
			throw new TypeMismatchException(value, requiredType, ex);
		}
		catch (IllegalStateException ex) {
			throw new ConversionNotSupportedException(value, requiredType, ex);
		}
		catch (IllegalArgumentException ex) {
			throw new TypeMismatchException(value, requiredType, ex);
		}
	}

}
