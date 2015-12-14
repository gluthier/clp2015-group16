object SuperAssing {
	def main(): Unit = {
		{}
	}
}

class A {
	def m(): Int = {
		var x: B;
		x = new A();
		return 1;
	}
}

class B extends A {
}
