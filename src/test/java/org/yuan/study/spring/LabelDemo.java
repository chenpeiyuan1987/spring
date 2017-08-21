package org.yuan.study.spring;

import org.junit.Test;

public class LabelDemo {

	@Test
	public void testLoop() {
		int count = 3;
		
		loop:
		for (int i=0; i<10; i++) {
			for (int j=0; j<10; j++ ) {
				System.out.print(i + "-" + j + ", ");
				if (j == 9 && count > 0) {
					System.out.println();
					count--;
					continue loop;
				}
			}
		}
	}
	
}
