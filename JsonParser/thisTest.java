class ThisTest {
	int a;
	ThisTest t;
	public ThisTest returnSelf() {
		return this;
	}

	public boolean returnInt(boolean a) {
		a = this.a > 2;
		return a;
	}
}