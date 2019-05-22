package ch.mse.app.diamond;

interface A {
	default String getValue() { return "A"; }
}

interface B extends A {
	default String getValue() { return "B"; }
}

interface C extends A {
//	default String getValue() { return "C"; }
}

public class D implements B, C {
	
//	public String getValue() {
//		return B.super.getValue();
//	}

	public static void main(String[] args) {
		D d = new D();
		String res = d.getValue();
		System.out.println(res);
		System.out.println("done2");
	}
}
