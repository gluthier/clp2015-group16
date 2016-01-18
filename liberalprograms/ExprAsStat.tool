object ExprAsStat {
	def main(): Unit = {
	    println(new Run().start());
	}
}

class Run {
    def start(): Int = {
    	var a: ComplexNumber;
        var b: ComplexNumber;
   	    var c: ComplexNumber;
   	    var d: ComplexNumber;
   	    var e: ComplexNumber;
   	    var x: Int;
   	    a = new ComplexNumber().init(2, 1);
   	    b = new ComplexNumber().init(3, 4);
   	    c = new ComplexNumber().init(1, 5);
   	    d = a.plus(b);
   	    e = c.plus(b);
        println("a = " + a.toString());
        println("b = " + b.toString());
        println("c = " + c.toString());
        println("d = " + d.toString());
        println("e = " + e.toString());
        5 + 5;
        "abc";
        e.setReal(5);
        return 0;
    }
}

class ComplexNumber {
    var real: Int;
    var imaginary: Int;

    def init(r: Int, i: Int): ComplexNumber = {
        real = r;
        imaginary = i;
        return this;
    }

    def toString(): String = {
        var operator: String;
        if (imaginary < 0) {
            operator = " - ";
        } else {
            operator = " + ";
        }
        return real + operator + imaginary + "i";
    }

    def getReal(): Int = {
        return real;
    }

    def getImaginary(): Int = {
        return imaginary;
    }

    def setReal(r: Int): Int = {
        real = r;
        return 0;
    }

    def plus(other: ComplexNumber): ComplexNumber = {
        var result: ComplexNumber;
        var resultReal: Int;
        var resultImaginary: Int;
        resultReal = real + other.getReal();
        resultImaginary = imaginary + other.getImaginary();
        result = new ComplexNumber().init(resultReal, resultImaginary);
        return result;
    }
}
