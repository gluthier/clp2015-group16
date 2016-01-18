object InfixMethods {
    def main() : Unit = {
        println(new Tester().test())
    }
}

class Tester {
    def test() : Int = {
		var myObject : Object
		myObject = new Object().init()
		return myObject coucou 6
    }
}

class Object {
	var value : Int

	def init() : Object = {
		value = 14
		return this
	}

	def coucou(a: Int) : Int = {
		return value + a
	}
}
