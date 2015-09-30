object BinaryTree {
    def main(): Unit = {
        println(new BT().Start(10));
    }
}

class BT {
    var number : Int[];
    var size : Int;
    var tree : Tree;

    def Start(sz : Int) : Int = {
        var aux01 : Int;
        var aux02 : Int;

        aux01 = this.Init(sz);
        aux02 = this.CreateTree(sz);

        return 0;
    }

    def CreateTree(sz : Int) : Int = {
        var counter : Int;
        var aux00 : Int;
        var aux01 : Int;

        tree = new Tree();

        aux00 = tree.setRoot(new None().init(0));
        counter = 0;
        while (counter < sz) {
            aux01 = tree.insert(number[counter], tree.getRoot(), new None().init(0));
            println("Inserted " + number[counter] + " into tree.");
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
}

class Tree {
    var root : Node;

    def setRoot(nd : Node) : Int = {
        root = nd;

        return 0;
    }

    def getRoot() : Node = {
        return root;
    }

    def insert(value : Int, root : Node, parent : Node) : Int = {
        var newNode : Node;
        var aux01 : Int;

        newNode = new Node().init(value);
        if (root.getType() == 0) {
            aux01 = this.setRoot(newNode);
        } else {
            if (root.getValue() < value) {
                aux01 = this.insert(value, root.getLeft(), root);
            } else {
                aux01 = this.insert(value, root.getRight(), root);
            }
        }
        return 0;
    }
}

class Node {
    var value : Int;
    var parent : Node;
    var left : Node;
    var right : Node;
    var type : Int;

    def init(nv : Int) : Node = {
        type = 1;
        value = nv;
        left = new None().init(0);
        right = new None().init(0);
        return this;
    }

    def getType() : Int = {
        return type;
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

    def setLeft(nd : Node) : Int = {
        left = nd;
        return 0;
    }

    def setRight(nd : Node) : Int = {
        right = nd;
        return 0;
    }
}

class Leaf extends Node {
    def init(nv : Int) : Node = {
        value = nv;
        left = new None();
        right = new None();
        type = 1;
        return this;
    }
}

class None extends Node {
    def init(nv : Int) : Node = {
        parent = this;
        left = this;
        right = this;
        type = 0;
        return this;
    }
}