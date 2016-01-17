object OperatorsAsMethods {
    def main() : Unit = {
        println(new Tester().test())
    }
}

class Tester {
    def test() : Int = {
		var a: ArbitraryObject
		var b: ArbitraryObject

		a = new ArbitraryObject().init()
		b = new ArbitraryObject().init()

		return a.+(b)
    }
}

class ArbitraryObject {
	def init() : ArbitraryObject = {
		return this;
	}

	def +(b: ArbitraryObject): Int = {
		return 100
	}
}
