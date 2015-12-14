object LeadigZero {
    def main() : Unit = {
        println(new Tester().run());
    }
}

class Tester {
    def run() : Int = {
        var intLit : Int;
        var intLit2 : Int;
        var intLit3 : Int;
        var intLit4 : Int;
        var intLit5 : Int;

        intLit = 002;
        intLit2 = 45;
        intLit3 = 0765;
        intLit4 = 0;
        intLit5 = 0000000000;

        return intLit;
    }
}
