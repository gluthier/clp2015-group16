object LinkedList {
    def main() : Unit = {
        println(new LL().Start(10));
    }
}

class LL {
    var number : Int[];
    var size : Int;
    var head : ListElement;

    def Start(sz : Int) : Int = {
        var aux01 : Int;
        var aux02 : Int;
        var counter : Int;

        aux01 = this.Init(sz);
        head = new ListElement().init(number[0]);
        counter = 1;
        while(counter < sz) {
            aux02 = this.prepend(number[counter], head);
            println("Added " + number[counter] + "to Linked List.");
            counter = counter + 1;
        }
        return 0;
    }

    // Initialize array of integers
    def Init(sz : Int) : Int = {
        size = sz ;
        number = new Int[sz] ;

        number[0] = 20 ;
        number[1] = 7  ;
        number[2] = 12 ;
        number[3] = 18 ;
        number[4] = 2  ;
        number[5] = 11 ;
        number[6] = 6  ;
        number[7] = 9  ;
        number[8] = 19 ;
        number[9] = 5  ;

        return 0 ;
    }

    def prepend(vl : Int, hd : ListElement) : Int = {
        var newHead : ListElement;
        var aux00 : Int;
        var aux01 : Int;

        newHead = new ListElement().init(vl);
        aux00 = newHead.setNext(hd);
        aux01 = hd.setPrevious(newHead);

        return 0;
    }
}

class ListElement {
    var value : Int;
    var previous : ListElement;
    var next : ListElement;

    def init(vl : Int) : ListElement = {
        value = vl;
        return this;
    }

    def setNext(nx : ListElement) : Int = {
        next = nx;
        return 0;
    }

    def setPrevious(pv : ListElement) : Int = {
        previous = pv;
        return 0;
    }

    def getPrevious() : ListElement = {
        return previous;
    }

    def getNext() : ListElement = {
        return next;
    }
}