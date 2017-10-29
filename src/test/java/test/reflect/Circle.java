package test.reflect;

public class Circle {
	private double radius;
	private Point point;
	
	public Circle(double radius, Point point) {
		this.point = point;
		this.radius = radius;
	}
	
	public double calc() {
		return Math.PI * radius * radius;
	}

	public static class Point {
		private double x;
		private double y;
		
		public Point(double x, double y) {
			this.x = x;
			this.y = y;
		}

		public double getX() {
			return x;
		}

		public double getY() {
			return y;
		}
	}
	
}
