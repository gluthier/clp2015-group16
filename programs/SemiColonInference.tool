object SemiColonInference {
    def main() : Unit = {
        println(new Tester().test())
    }
}

class Tester {
    def test() : Int = {
		var a : Int
		var b : Int
		var c : Int
		var d : Int

		a = 1
		b = 2

		c = a +
			b

		d =
			a
			+ b
			* 4

		return
		c
    }
}
