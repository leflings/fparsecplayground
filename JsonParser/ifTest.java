class IfClass extends ElseClass {
	public static void test() {
		if(true)
			a = 3;
		else
			a = 4;

		if(false) {
			b();
		} else if(true) {
			what();
		}

		if(false) {
			b();
		} else if(true) {
			what();
		} else if (3 < 2) {
			no();
		} else if (5 > 6) {
		} else if (3 < 2) {
			p();
		} else {
			return;
		}
	}
}


