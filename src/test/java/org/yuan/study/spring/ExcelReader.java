package org.yuan.study.spring;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.openxmlformats.schemas.spreadsheetml.x2006.main.CTCell;

public class ExcelReader {

	public List<List<Object>> read(String filePath) {
		List<List<Object>> result = new ArrayList<List<Object>>();
		
		try {
			FileInputStream fis = new FileInputStream(filePath);
			//POIFSFileSystem pfs = new POIFSFileSystem(fis);
			XSSFWorkbook hwb = new XSSFWorkbook(fis);
			
			XSSFSheet sheet = hwb.getSheetAt(0);
			int rowLength = sheet.getLastRowNum() + 1;
			for (int i = 0; i < rowLength; i++) {
				List<Object> list = new ArrayList<Object>();
				result.add(list);
				
				XSSFRow row = sheet.getRow(i);
				int colLength = row.getLastCellNum();
				for (int j = 0; j < colLength; j++) {
					XSSFCell cell = row.getCell(j);
					list.add(cell.toString());
				}
			}
			
			return result;
		}
		catch (IOException ex) {
			ex.printStackTrace();
		}
		
		return null;
	}
	
	private String getStringValue(XSSFCell cell) {
		switch (cell.getCellType()) {
		case XSSFCell.CELL_TYPE_BOOLEAN:
			return String.valueOf(cell.getBooleanCellValue());
		case XSSFCell.CELL_TYPE_NUMERIC:
			return String.valueOf(cell.getNumericCellValue());
		default:
			return String.valueOf(cell.getStringCellValue());
		}
	}
	
	public static void main(String[] args) {
		ExcelReader reader = new ExcelReader();
		List<List<Object>> result = reader.read("C:/Users/Yuan/Desktop/KPV/临时区域总代名单.xlsx");
		
		for (List<Object> list : result) {
			System.out.println(list);
		}
	}
}
