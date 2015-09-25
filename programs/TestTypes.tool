/*
 * This program tests tool's types and some opperations on them.
 */
object TestTypes {
    def main() : Unit = {
        println(new Tester().test());
    }
}

class Tester {
    def test() : String = {
        var a : IntTest;
        var b : IntTest;
        var c : IntTest;
        var e : IntArrayTest;
        var t : BoolTest;
        var f : BoolTest;
        var o : BoolTest;
        var s1 : StringTest;
        var s2 : StringTest;

        a = new IntTest().init(3); //a=3
        b = new IntTest().init(4); //b=4
        c = new IntTest().init(0); //c=0
        e = new IntArrayTest().init(5);
        t = new BoolTest().init(true); //t=true
        f = new BoolTest().init(false); //f=false
        o = new BoolTest().init(true); //o=true
        s1 = new StringTest().init("hello"); //s1=hello
        s2 = new StringTest().init("world"); //s2=world

        println(this.testIntEquality(a, 3));
        println(this.testBoolEquality(t, true));
        println(this.testIntSmallerThan(a, 5));
        println(this.testIntSmallerThan(b, 2));

        c = a.add(b); //c=7
        println(this.testIntEquality(c, 7));
        c = c.subtract(a); //c=4
        println(this.testIntEquality(c, 4));
        c = c.multiply(b); //c=16
        println(this.testIntEquality(c, 16));
        c = c.divide(a); //c=3
        println(this.testIntEquality(c, 5));

        o = t.and(f); //o=false
        println(this.testBoolEquality(o, false));
        o = t.or(f); //o=true;
        println(this.testBoolEquality(o, true));
        o = t.negate(); //o=false
        println(this.testBoolEquality(o, false));

        println(s1.append(s2));

        println(">test length of array");
        println(this.testIntArrayLength(e, 5));

        return "bye.";
    }

    def testIntSmallerThan(tested : IntTest, const : Int) :  String = {
        var response : String;
        if (tested.getInt() < const) {
            response = tested.getInt() + " is smaller than " + const;
        } else {
            response = tested.getInt() + " is not smaller than " + const;
        }
        return response;
    }

    def testIntEquality(tested : IntTest, const : Int) : String = {
        var response : String;
        if (tested.getInt() == const) {
            response = tested.getInt() + " equals " + const;
        } else {
            response = tested.getInt() + " does not equal " + const;
        }
        return response;
    }

    def testIntArrayLength(tested : IntArrayTest, const : Int) : String = {
        var response : String;
        if (tested.getLength() == const) {
            response = tested.getLength() + " equals " + const;
        } else {
            response = tested.getLength() + " does not equal " + const;
        }
        return response;
    }

    def testBoolEquality(tested : BoolTest, const : Bool) : String = {
        var response : String;
        var constString : String;

        if (const) {
            constString = "true";
        } else {
            constString = "false";
        }

        if (tested.getBool() == const) {
            response = tested.getBoolString() + " equals " + constString;
        } else {
            response = tested.getBoolString() + " does not equal " + constString;
        }

        return response;
    }
}

class IntTest {
    var v : Int;

    def init(val : Int) : IntTest = {
        v = val;
        return this;
    }

    def getInt() : Int = {
        return v;
    }

    def add(t : IntTest) : IntTest = {
        println(">"+v+"+"+t.getInt());
        return new IntTest().init(v + t.getInt());
    }

    def subtract(t : IntTest) : IntTest = {
        println(">"+v+"-"+t.getInt());
        return new IntTest().init(v - t.getInt());
    }

    def multiply(t : IntTest) : IntTest = {
        println(">"+v+"*"+t.getInt());
        return new IntTest().init(v * t.getInt());
    }

    def divide(t : IntTest) : IntTest = {
        println(">"+v+"/"+t.getInt());
        return new IntTest().init(v / t.getInt());
    }
}

class IntArrayTest {
    var v : Int[];

    def init(n : Int) : IntArrayTest = {
        var i : Int;
        v = new Int[n];
        i = 0;
        while (i < v.length) {
            v[i] = i;
            i = i+1;
        }
        return this;
    }

    def getArray() : Int[] = {
        return v;
    }

    def getLength() : Int = {
        return v.length;
    }
}

class BoolTest {
    var v : Bool;

    def init(val : Bool) : BoolTest = {
        v = val;
        return this;
    }

    def getBool() : Bool = {
        return v;
    }

    def getBoolString() : String = {
        var response : String;
        if(v) {
            response = "true";
        } else {
            response = "false";
        }
        return response;
    }

    def and(t : BoolTest) : BoolTest = {
        println(">"+this.getBoolString()+"&&"+t.getBoolString());
        return new BoolTest().init(v && t.getBool());
    }

    def or(t : BoolTest) : BoolTest = {
        println(">"+this.getBoolString()+"||"+t.getBoolString());
        return new BoolTest().init(v || t.getBool());
    }

    def negate() : BoolTest = {
        println(">!"+this.getBoolString());
        return new BoolTest().init(!v);
    }
}

class StringTest {
    var v : String;

    def init(val : String) : StringTest = {
        v = val;
        return this;
    }

    def getString() : String = {
        return v;
    }

    def append(t : StringTest) : String = {
        println(">append '"+t.getString()+"' to '"+v+"'");
        return v+t.getString();
    }
}
