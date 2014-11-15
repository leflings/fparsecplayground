class main {
	IntStack stack;
	public static void main(String[] args) {
		stack.init();
		int a;
		a = 0;
		while( a < 5 ) {
			stack.push(a);
			a = a + 1;
		}
		main(stack.stack);
		int x;
		A aclass;
		x = (new A[3])[2].bs[3].getC();
	}

	public static int main(int[] a) {
		return 5 * 3 + 2 * 5;
	}

	public static void main(int a) {
	}
}

class A {
	B b;
	B[] bs;
	public B getB() {
		return b;
	}
}

class B extends A {
	int c;
	int getC() {
		return c;
	}
}

class IntStack {
	int[] stack;
	int i;
	public void init() {
		stack = new int[5000];
		i = 0;
	}

	public void push(int a) {
		stack[i] = a;
		i = i + 1;
	}

	public int pop() {
		i = i - 1;
		return stack[i+1];
	}
}
