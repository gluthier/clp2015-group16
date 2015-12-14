object TestPriority {
    def main() : Unit = {
        println(new Priority().test());
    }
}

class Priority {
    def test() : String = {
        var arr: Int[];
        println(false && false || true && true || false);
        println(true || false && false);
        println(!(1 < 2 || 1 == 1));
        println(true || false && 1 == 1 < 2 -1 + 4 / 2 * 1);
        println(1 == 2 < 3);
        println(1 < 2 == 3);
        println(1 < 2 || true);
        println(1 < 2 * 3);
        println(1 < 2 + 3 * 4 - 2);
        println(1 + 2 * 3);
        println(1 * 2 + 3);
        arr[0]=2;
        println(arr[0] * 2);
        println(arr.length / 2);
        println((!((1 < 2147483647)) && !((1 == 2147483647))));
        println(!(1 < 2147483647) && !(1 == 2147483647));
        return "tested";
    }
}

// !, then * and /, then + and -, then < and ==, then &&, then ||.
