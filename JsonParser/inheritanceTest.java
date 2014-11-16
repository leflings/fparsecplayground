class A {
	int a;
	public int getA(boolean inverse) {
		if(inverse) {
			a = 2
			return -a;
		}
		return a;
	}
}
class B extends A {
	int b;
	public int getAB() {
		return a+getA(true)+b;
	}
}

class C extends B {
	public boolean bIsBigger() {
		return b > a;
	}
}

class D {
	public boolean test() {
		return (new C()).bIsBigger();
	}
}
