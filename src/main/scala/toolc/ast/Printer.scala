package toolc
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
      t match {
          case x: Program =>
            val classes: StringBuilder = new StringBuilder
            for (c <- x.classes) {
                classes append apply(c) + "\n"
            }
            apply(x.main) + "\n" + classes.mkString
          case x: MainObject =>
            val stats: StringBuilder = new StringBuilder
            for (s <- x.stats) {
                stats append apply(s) + "\n"
            }
            "object " + x.id + " {\n def main(): Unit = {" + stats.mkString + "}}"
          case x: ClassDecl =>
            val vars: StringBuilder = new StringBuilder
            for (v <- x.vars) {
                vars append apply(v) + "\n"
            }
            val meths: StringBuilder = new StringBuilder
            for (m <- x.methods) {
                meths append apply(m)
            }
            val id: Identifier = x.id
            x.parent match {
                case Some(parent) =>
                    "class " + id + " extends " + parent + " {\n" + vars.mkString + "\n" + meths.mkString + "\n}"
                case None =>
                    "class " + id + " {\n" + vars.mkString + "\n" + meths.mkString + "\n}"
            }
          case x: VarDecl =>
            "var " + apply(x.id) + ": " + apply(x.tpe) + ";"
          case x: MethodDecl =>
            val args: StringBuilder= new StringBuilder
            if (x.args.nonEmpty) {
                args append apply(x.args.head)
                for (a <- x.args.tail) {
                    args append ", " + apply(a)
                }
            }
            val vars: StringBuilder = new StringBuilder
            for (v <- x.vars) {
                vars append apply(v) + "\n"
            }
            val stats: StringBuilder = new StringBuilder
            for (s <- x.stats) {
                stats append apply(s) + "\n"
            }

            "def " + x.id + "(" + args.mkString + "): " + x.retType + " = {\n" + vars.mkString + "\n" + stats.mkString + "\nreturn " + x.retExpr
          case x: Formal =>
            apply(x.id) + ": " + apply(x.tpe)
          case x: IntArrayType =>
            "Int[]"
          case x: IntType =>
            "Int"
          case x: BooleanType =>
            "Bool"
          case x: StringType =>
            "String"
          case x: Block =>
            val stats: StringBuilder = new StringBuilder
            for(s <- x.stats) {
                stats append apply(s)
            }
            "{\n" + stats.mkString + "\n}"
          case x: If =>
            val expr: ExprTree = x.expr
            val thn: StatTree = x.thn
            x.els match {
                case Some(els) =>
                  "if (" + apply(expr) + ") {\n" + apply(thn) + "\n} else {\n" + apply(els) + "\n}"
                case None =>
                  "if (" + apply(expr) + ") {\n" + apply(thn) + "\n}"
            }

          case x: While =>
            "while (" + apply(x.expr) + ") {\n" + apply(x.stat) + "\n}"
          case x: Println =>
            "println(" + apply(x.expr) + ");"
          case x: Assign =>
            apply(x.id) + " = " + apply(x.expr) + ";"
          case x: ArrayAssign =>
            apply(x.id) + "[" + apply(x.index) + "]" + " = " + apply(x.expr) + ";"
          case x: And =>
            apply(x.lhs) + " && " + apply(x.rhs)
          case x: Or =>
            apply(x.lhs) + "|| " + apply(x.rhs)
          case x: Plus =>
            apply(x.lhs) + " + " + apply(x.rhs)
          case x: Minus =>
            apply(x.lhs) + " - " + apply(x.rhs)
          case x: Times =>
            apply(x.lhs) + " * " + apply(x.rhs)
          case x: Div =>
            apply(x.lhs) + " / " + apply(x.rhs)
          case x: LessThan =>
            apply(x.lhs) + " < " + apply(x.rhs)
          case x: Equals =>
            apply(x.lhs) + " == " + apply(x.rhs)
          case x: ArrayRead =>
            apply(x.arr) + "[" + apply(x.index) + "]"
          case x: ArrayLength =>
            apply(x.arr) + ".length"
          case x: MethodCall =>
            val args: StringBuilder = new StringBuilder
            if (x.args.nonEmpty) {
                args append apply(x.args.head)
                for (a <- x.args.tail) {
                    args append ", " + apply(a)
                }
            }
            apply(x.obj) + "." + apply(x.meth) + "(" + args.mkString+ ")"
          case x: IntLit =>
            x.value.toString
          case x: StringLit =>
            x.value
          case x: True =>
            "true"
          case x: False =>
            "false"
          case x: Identifier =>
            x.value
          case x: This =>
            "this"
          case x: NewIntArray =>
            "new Int [" + apply(x.size) +  "]"
          case x: New =>
            "new " + apply(x.tpe) + "()"
          case x: Not =>
            "!" + apply(x.expr)
          case _ => sys.error("error")
      }
  }
}
