object WrongTypes {
	def main(): Unit = {
		println(new Computer().test());
	}
}

class Computer {
	def test(): String = {
		var a: Int;
		var b: String;
		a = 1;
		b = a;

		return b;
	}
}
