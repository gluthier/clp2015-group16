package toolc
package ast

import Trees._
import toolc.utils.{Context, Pipeline}

object Printer extends Pipeline[Program, String] {
  def apply(t: Tree): String = {
    t match {
      case Program(main, classes) =>
        val builder: StringBuilder = new StringBuilder
        for (c <- classes) {
            builder.append(apply(c) + "\n")
        }
        apply(main) + "\n" + builder.mkString
      case MainObject(id, stats) =>
        val builder: StringBuilder = new StringBuilder
        for (s <- stats) {
            builder.append(apply(s) + "\n")
        }
        "object " + apply(id) + " {\ndef main(): Unit = {\n" + builder.mkString + "}\n}"
      case ClassDecl(id, parent, vars, methods) =>
        val builder_vars: StringBuilder = new StringBuilder
        for (v <- vars) {
            builder_vars.append(apply(v) + "\n")
        }
        val builder_meths: StringBuilder = new StringBuilder
        for (m <- methods) {
            builder_meths.append(apply(m) + "\n")
        }
        parent match {
          case Some(parent) =>
            "class " + apply(id) + " extends " + apply(parent) + " {\n" + builder_vars.mkString + builder_meths.mkString + "}"
          case None =>
            "class " + apply(id) + " {\n" + builder_vars.mkString + builder_meths.mkString + "}"
        }
      case VarDecl(tpe, id) =>
        "var " + apply(id) + ": " + apply(tpe) + ";"
      case MethodDecl(retType, id, args, vars, stats, retExpr) =>
        val builder_args: StringBuilder = new StringBuilder
        if (args.nonEmpty) {
            builder_args.append(apply(args.head))
          for (a <- args.tail) {
              builder_args.append(", " + apply(a))
          }
        }
        val builder_vars: StringBuilder = new StringBuilder
        for (v <- vars) {
            builder_vars.append(apply(v) + "\n")
        }
        val builder_stats: StringBuilder = new StringBuilder
        for (s <- stats) {
            builder_stats.append(apply(s) + "\n")
        }

        "def " + apply(id) + "(" + builder_args.mkString + "): " + apply(retType) + " = {\n" + builder_vars.mkString + builder_stats.mkString + "return " + apply(retExpr) + ";\n}\n"
      case Formal(tpe, id) =>
        apply(id) + ": " + apply(tpe)
      case IntArrayType() =>
        "Int[]"
      case IntType() =>
        "Int"
      case BooleanType() =>
        "Bool"
      case StringType() =>
        "String"
      case Block(stats) =>
        val builder_stats: StringBuilder = new StringBuilder
        for (s <- stats) {
            builder_stats.append(apply(s) + "\n")
        }
        builder_stats.mkString
      case If(expr, thn, els) =>
        els match {
          case Some(els) =>
            "if (" + apply(expr) + ") {\n" + apply(thn) + "} else {\n" + apply(els) + "}"
          case None =>
            "if (" + apply(expr) + ") {\n" + apply(thn) + "}"
        }
      case While(expr, stat) =>
        "while (" + apply(expr) + ") {\n" + apply(stat) + "}"
      case Println(expr) =>
        "println(" + apply(expr) + ");"
      case Assign(id, expr) =>
        apply(id) + " = " + apply(expr) + ";"
      case ArrayAssign(id, index, expr) =>
        apply(id) + "[" + apply(index) + "]" + " = " + apply(expr) + ";"
      case And(lhs, rhs) =>
        "(" + apply(lhs) + " && " + apply(rhs) + ")"
      case Or(lhs, rhs) =>
        "(" + apply(lhs) + "|| " + apply(rhs) + ")"
      case Plus(lhs, rhs) =>
        "(" + apply(lhs) + " + " + apply(rhs) + ")"
      case Minus(lhs, rhs) =>
        "(" + apply(lhs) + " - " + apply(rhs) + ")"
      case Times(lhs, rhs) =>
        "(" + apply(lhs) + " * " + apply(rhs) + ")"
      case Div(lhs, rhs) =>
        "(" + apply(lhs) + " / " + apply(rhs) + ")"
      case LessThan(lhs, rhs) =>
        "(" + apply(lhs) + " < " + apply(rhs) + ")"
      case Equals(lhs, rhs) =>
        "(" + apply(lhs) + " == " + apply(rhs) + ")"
      case ArrayRead(arr, index) =>
        apply(arr) + "[" + apply(index) + "]"
      case ArrayLength(arr) =>
        apply(arr) + ".length"
      case MethodCall(obj, meth, args) =>
        val builder_args: StringBuilder = new StringBuilder
        if (args.nonEmpty) {
            builder_args.append(apply(args.head))
          for (a <- args.tail) {
              builder_args.append(", " + apply(a))
          }
        }
        apply(obj) + "." + apply(meth) + "(" + builder_args.mkString + ")"
      case IntLit(value) =>
        value.toString
      case StringLit(value) =>
        "\"" + value + "\""
      case True() =>
        "true"
      case False() =>
        "false"
      case Identifier(value) =>
        value
      case This() =>
        "this"
      case NewIntArray(size) =>
        "new Int [" + apply(size) + "]"
      case New(tpe) =>
        "new " + apply(tpe) + "()"
      case Not(expr) =>
        "!(" + apply(expr) + ")"
    }
  }
  def run(ctx: Context)(v: Program): String = apply(v)
}
