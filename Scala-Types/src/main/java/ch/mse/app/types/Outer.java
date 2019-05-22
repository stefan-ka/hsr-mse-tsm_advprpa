package ch.mse.app.types;

public class Outer {

	class Inner {
	}

	public void put(Inner inner) {

	}

	public static void main(String[] args) {
		Outer outer1 = new Outer();
		Inner inner1 = outer1.new Inner();

		Outer outer2 = new Outer();
		Inner inner2 = outer2.new Inner();

		outer1.put(inner2);
		outer2.put(inner1);
	}
}
