package toolc
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
      t match {
          case Program =>
            val classes: StringBuilder = new StringBuilder
            for (c <- t.classes) {
                classes append apply(c) + "\n"
            }
            apply(t.main) + "\n" + classes.mkString
          case MainObject =>
            val stats: StringBuilder = new StringBuilder
            for (s <- t.stats) {
                stats append apply(s) + "\n"
            }
            "object " + t.id + " {\n def main(): Unit = {" + stats.mkString + "}}"
          case ClassDecl =>
            val vars: StringBuilder = new StringBuilder
            for (v <- t.vars) {
                vars append apply(v) + "\n"
            }
            val meths: StringBuilder = new StringBuilder
            for (m <- t.methods) {
                meths append apply(m)
            }
            t.parent match {
                case Some(x) =>
                    "class " + t.id + " extends " + t.parent + " {\n" + vars.mkString + "\n" + meths.mkString + "\n}"
                case None =>
                    "class " + t.id + " {\n" + vars.mkString + "\n" + meths.mkString + "\n}"
            }
          case VarDecl =>
            "var " + apply(t.id) + ": " + apply(t.tpe) + ";"
          case MethodDecl =>
            val args: StringBuilder= new StringBuilder
            if (t.args.length > 0) {
                args append apply(t.args[0])
                for (i <- 1 to t.args.length) {
                    args append ", " + apply(t.args[i])
                }
            }
            val vars: StringBuilder = new StringBuilder
            for (v <- t.vars) {
                vars append apply(v) + "\n"
            }
            val stats: StringBuilder = new StringBuilder
            for (s <- t.stats) {
                stats append apply(s) + "\n"
            }

            "def " + t.id + "(" + args.mkString + "): " + t.retType + " = {\n" + vars.mkString + "\n" + stats.mkString + "\nreturn " + t.retExpr
          case Formal =>
            apply(t.id) + ": " + apply(t.tpe)
          case IntArrayType =>
            "Int[]"
          case IntType =>
            "Int"
          case BooleanType =>
            "Bool"
          case StringType =>
            "String"
          case Block =>
            val stats: StringBuilder = new StringBuilder
            for(s <- t.stats) {
                stats append apply(s)
            }
            "{\n" + stats.mkString + "\n}"
          case If =>
            t.els match {
                case Some(x) =>
                  "if (" + apply(t.expr) + ") {\n" + apply(t.thn) + "\n} else {\n" + apply(t.els) + "\n}"
                case None =>
                  "if (" + apply(t.expr) + ") {\n" + apply(t.thn) + "\n}"
            }

          case While =>
            "while (" + apply(t.expr) + ") {\n" + apply(t.stat) + "\n}"
          case Println =>
            "println(" + apply(t.expr) + ");"
          case Assign =>
            apply(t.id) + " = " + apply(t.expr) + ";"
          case ArrayAssign =>
            apply(t.id) + "[" + apply(t.index) + "]" + " = " + apply(t.expr) + ";"
          case And =>
            apply(t.lhs) + " && " + apply(t.rhs)
          case Or =>
            apply(t.lhs) + "|| " + apply(t.rhs)
          case Plus =>
            apply(t.lhs) + " + " + apply(t.rhs)
          case Minus =>
            apply(t.lhs) + " - " + apply(t.rhs)
          case Times =>
            apply(t.lhs) + " * " + apply(t.rhs)
          case Div =>
            apply(t.lhs) + " / " + apply(t.rhs)
          case LessThan =>
            apply(t.lhs) + " < " + apply(t.rhs)
          case Equals =>
            apply(t.lhs) + " == " + apply(t.rhs)
          case ArrayRead =>
            apply(t.arr) + "[" + apply(t.index) + "]"
          case ArrayLength =>
            apply(t.arr) + ".length"
          case MethodCall =>
            val args: StringBuilder = new StringBuilder
            if (t.args.length > 0) {
                args append apply(t.args[0])
                for (i <- 1 to t.args.length) {
                    args append ", " + apply(t.args[i])
                }
            }
            apply(t.obj) + "." + apply(t.meth) + "(" + args.mkString+ ")"
          case IntLit =>
            t.value
          case StringLit =>
            t.value
          case True =>
            "true"
          case False =>
            "false"
          case Identifier =>
            t.value
          case This =>
            "this"
          case NewIntArray =>
            "new Int [" + apply(t.size) +  "]"
          case New =>
            "new " + apply(t.tpe) + "()"
          case Not =>
            "!" + apply(t.expr)
          case _ => error("error")
      }
  }
}
