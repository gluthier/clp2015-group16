object BinaryTree {
    def main(): Unit = {
        println(new BT().Start())
    }
}

class BT {
    var number : Int[];
    var size : Int;
    var tree : Tree;

    def Start(sz : Int) : Int[] = {
        var aux01 : Int;
        var aux02 : Int;

        aux01 = this.Init(sz);
        aux02 = this.CreateTree(sz);
    }

    def CreateTree(sz : Int) : Int = {
        var counter : Int;
        var aux00 : Int;

        aux00 = tree.setRoot(None());
        counter = 0;
        while (counter < sz) {

        }
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
}

class Tree {
    var root : Node;

    def setRoot(nd : Node) : Int = {
        this.root = nd;

        return 0;
    }

    def getRoot() : Node = {
        return this.root;
    }

    def insert(value : Int, root : Node) : Int = {
        var newNode : Node;
        newNode = Node(value);
        if (root.getType() == 0) {
            tree.setRoot(newNode);
        } else {
            if (root.getValue() < value) {
                insert(value, root.getLeft())
            }
        }
    }
}

class Node {
    var value : Int;
    var parent : Node;
    var left : Node;
    var right : Node;
    var type : Int;

    def getType() : Int = {
        return this.type;
    }

    def init(nv : Int) : Node = {
        value = nv;
        return this;
    }

    def getValue() : Int = {
        return value;
    }

    def getLeft() : Node = {
        return left;
    }

    def getRight() : Node = {
        return right;
    }
}

class Leaf extends Node {
    left = None();
    right = None();
    type = 1;
}

class None extends Node {
    parent = this;
    left = this;
    right = this;
    type = 0;
}