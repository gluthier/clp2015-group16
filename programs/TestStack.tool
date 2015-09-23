object TestStack {
    def main() : Unit = {
        println(new Tester().test());
    }
}

class Tester {
    def test() : String = {
        var stack  : Stack;
        var head : Int;

        println(">Initialize stack:");
        stack = new Stack().init();
        println("  Stack: " + stack.printStack());

        println(">Push 3, 7, 15, 2:");
        stack = stack.push(3);
        stack = stack.push(7);
        stack = stack.push(15);
        stack = stack.push(2);
        println("  Stack: " + stack.printStack());

        println(">Pop twice:");
        head = stack.pop();
        println("  Element popped: " + head);
        head = stack.pop();
        println("  Element popped: " + head);
        println("  Stack: " + stack.printStack());

        return ">Exit";
    }
}

class Stack {
    var head : Frame;
    var nil : Frame;

    def init() : Stack = {
        nil = new Nil().init();
        head = nil;
        return this;
    }

    def push(x : Int) : Stack = {
        var newFrame : Frame;
        newFrame = new Frame().init();
        newFrame = newFrame.setFrame(x, head);
        head = newFrame;
        return this;
    }

    def pop() : Int = {
        var data : Int;
        data = 0;
        if (head == nil) {
            println("Underflow error: head is nil");
        } else {
            data = head.getData();
            head = head.getNext();
        }
        return data;
    }

    def printStack() : String = {
        var currentFrame : Frame;
        var response : String;

        currentFrame = head;

        if (currentFrame == nil) {
            response = "Empty Stack";
        } else {
            response = "(";
            while(!(currentFrame == nil)) {
                response = response + " " + currentFrame.getData();
                currentFrame = currentFrame.getNext();
            }
            response = response + " )";
        }

        return response;
    }
}

class Frame {
    var data : Int;
    var next : Frame;

    def init() : Frame = {
        next = new Nil().init();
        return this;
    }

    def getData(): Int = {
        return data;
    }

    def getNext(): Frame = {
        return next;
    }

    def setFrame(data_ : Int, next_ : Frame) : Frame = {
        data = data_;
        next = next_;
        return this;
    }

    def isNil() : Bool = {
        return false;
    }
}

class Nil extends Frame {
    def init() : Frame = {
        next = this;
        return this;
    }

    def isNil() : Bool = {
        return true;
    }
}
