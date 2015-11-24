package toolc
package ast

import Trees._
import toolc.utils.{Context, Pipeline}

object Printer extends Pipeline[Program, String] {
    var counter : Int = 0

    def apply(t: Tree): String = {
        def applyWithIndent(t: Tree, indentLvl: String): String = {
            t match {
                case Program(main, classes) =>
                    val builder: StringBuilder = new StringBuilder
                    for (c <- classes) {
                        builder.append(applyWithIndent(c, indentLvl) + "\n")
                    }
                    applyWithIndent(main, indentLvl) + "\n" + builder.mkString
                case MainObject(id, stats) =>
                    val builder: StringBuilder = new StringBuilder
                    for (s <- stats) {
                        builder.append(applyWithIndent(s, indentLvl + "\t\t") + "\n")
                    }
                    "object " + applyWithIndent(id, indentLvl) + " {\n\tdef main(): Unit = {\n" + builder.mkString + "\t}\n}"
                case ClassDecl(id, parent, vars, methods) =>
                    val builder_vars: StringBuilder = new StringBuilder
                    for (v <- vars) {
                        builder_vars.append(applyWithIndent(v, indentLvl + "\t") + "\n")
                    }
                    val builder_meths: StringBuilder = new StringBuilder
                    for (m <- methods) {
                        builder_meths.append(applyWithIndent(m, indentLvl + "\t") + "\n")
                    }
                    parent match {
                        case Some(par) =>
                        "class " + applyWithIndent(id, indentLvl) + " extends " + applyWithIndent(par, indentLvl) + " {\n" + builder_vars.mkString + builder_meths.mkString + "}"
                        case None =>
                        "class " + applyWithIndent(id, indentLvl) + " {\n" + builder_vars.mkString + builder_meths.mkString + "}"
                    }
                case VarDecl(tpe, id) =>
                    indentLvl + "var " + applyWithIndent(id, indentLvl) + ": " + applyWithIndent(tpe, indentLvl) + ";"
                case MethodDecl(retType, id, args, vars, stats, retExpr) =>
                    val builder_args: StringBuilder = new StringBuilder
                    if (args.nonEmpty) {
                        builder_args.append(applyWithIndent(args.head, indentLvl))
                        for (a <- args.tail) {
                            builder_args.append(", " + applyWithIndent(a, indentLvl))
                        }
                    }
                    val builder_vars: StringBuilder = new StringBuilder
                    for (v <- vars) {
                        builder_vars.append(applyWithIndent(v, indentLvl + "\t") + "\n")
                    }
                    val builder_stats: StringBuilder = new StringBuilder
                    for (s <- stats) {
                        builder_stats.append(applyWithIndent(s, indentLvl + "\t") + "\n")
                    }

                    indentLvl + "def " + applyWithIndent(id, indentLvl) + "(" + builder_args.mkString + "): " + applyWithIndent(retType, indentLvl) + " = {\n" + builder_vars.mkString + builder_stats.mkString + "\n" +
                    indentLvl + "\treturn " + applyWithIndent(retExpr, indentLvl + "\t") + ";\n" +
                    indentLvl + "}"
                case Formal(tpe, id) =>
                    applyWithIndent(id, indentLvl) + ": " + applyWithIndent(tpe, indentLvl)
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
                        builder_stats.append(applyWithIndent(s, indentLvl) + "\n")
                    }
                    builder_stats.mkString
                case If(expr, thn, els) =>
                    els match {
                        case Some(elsi) =>
                            indentLvl + "if (" + applyWithIndent(expr, indentLvl) + ") {\n" +
                            applyWithIndent(thn, indentLvl + "\t") + "\n" +
                            indentLvl + "} else {\n" +
                            applyWithIndent(elsi, indentLvl + "\t") + "\n" + indentLvl + "}"
                        case None =>
                            indentLvl + "if (" + applyWithIndent(expr, indentLvl) + ") {\n" +
                            applyWithIndent(thn, indentLvl + "\t") + "\n" + indentLvl + "}"
                    }
                case While(expr, stat) =>
                    indentLvl + "while (" + applyWithIndent(expr, indentLvl) + ") {\n" +
                    applyWithIndent(stat, indentLvl + "\t") + "\n" + indentLvl + "}"
                case Println(expr) =>
                    indentLvl + "println(" + applyWithIndent(expr, indentLvl) + ");"
                case Assign(id, expr) =>
                    indentLvl + applyWithIndent(id, indentLvl) + " = " + applyWithIndent(expr, indentLvl) + ";"
                case ArrayAssign(id, index, expr) =>
                    indentLvl + applyWithIndent(id, indentLvl) + "[" + applyWithIndent(index, indentLvl) + "]" + " = " + applyWithIndent(expr, indentLvl) + ";"
                case Not(expr) =>
                    "!(" + applyWithIndent(expr, indentLvl) + ")"
                case Times(lhs, rhs) =>
                    "(" + applyWithIndent(lhs, indentLvl) + " * " + applyWithIndent(rhs, indentLvl) + ")"
                case Div(lhs, rhs) =>
                    "(" + applyWithIndent(lhs, indentLvl) + " / " + applyWithIndent(rhs, indentLvl) + ")"
                case Plus(lhs, rhs) =>
                    "(" + applyWithIndent(lhs, indentLvl) + " + " + applyWithIndent(rhs, indentLvl) + ")"
                case Minus(lhs, rhs) =>
                    "(" + applyWithIndent(lhs, indentLvl) + " - " + applyWithIndent(rhs, indentLvl) + ")"
                case LessThan(lhs, rhs) =>
                    "(" + applyWithIndent(lhs, indentLvl) + " < " + applyWithIndent(rhs, indentLvl) + ")"
                case Equals(lhs, rhs) =>
                    "(" + applyWithIndent(lhs, indentLvl) + " == " + applyWithIndent(rhs, indentLvl) + ")"
                case And(lhs, rhs) =>
                    "(" + applyWithIndent(lhs, indentLvl) + " && " + applyWithIndent(rhs, indentLvl) + ")"
                case Or(lhs, rhs) =>
                    "(" + applyWithIndent(lhs, indentLvl) + " || " + applyWithIndent(rhs, indentLvl) + ")"
                case ArrayRead(arr, index) =>
                    applyWithIndent(arr, indentLvl) + "[" + applyWithIndent(index, indentLvl) + "]"
                case ArrayLength(arr) =>
                    applyWithIndent(arr, indentLvl) + ".length"
                case MethodCall(obj, meth, args) =>
                    val builder_args: StringBuilder = new StringBuilder
                    if (args.nonEmpty) {
                        builder_args.append(applyWithIndent(args.head, indentLvl))
                        for (a <- args.tail) {
                            builder_args.append(", " + applyWithIndent(a, indentLvl))
                        }
                    }
                    applyWithIndent(obj, indentLvl) + "." + applyWithIndent(meth, indentLvl) + "(" + builder_args.mkString + ")"
                case IntLit(value) =>
                    value.toString
                case StringLit(value) =>
                    "\"" + value + "\""
                case True() =>
                    "true"
                case False() =>
                    "false"
                case id : Identifier =>
                    if (id.hasSymbol) id.value + "#" + id.getSymbol.id
                    else id.value + "#??"
                case This() =>
                    "this"
                case NewIntArray(size) =>
                    "new Int [" + applyWithIndent(size, indentLvl) + "]"
                case New(tpe) =>
                    "new " + applyWithIndent(tpe, indentLvl) + "()"
            }
        }

        applyWithIndent(t, "")
        /*
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
                    builder.append("\t\t" + apply(s) + "\n")
                }
                "object " + apply(id) + " {\n\tdef main(): Unit = {\n" + builder.mkString + "\t}\n}"
            case ClassDecl(id, parent, vars, methods) =>
                val builder_vars: StringBuilder = new StringBuilder
                for (v <- vars) {
                    builder_vars.append("\t" + apply(v) + "\n")
                }
                val builder_meths: StringBuilder = new StringBuilder
                for (m <- methods) {
                    builder_meths.append("\t" + apply(m) + "\n")
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
                    builder_vars.append("\t\t" + apply(v) + "\n")
                }
                val builder_stats: StringBuilder = new StringBuilder
                for (s <- stats) {
                    builder_stats.append("\t\t" + apply(s) + "\n")
                }

                "def " + apply(id) + "(" + builder_args.mkString + "): " + apply(retType) + " = {\n" + builder_vars.mkString + builder_stats.mkString + "\t\treturn " + apply(retExpr) + ";\n\t}"
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
                    "if (" + apply(expr) + ") {\n\t" + apply(thn) + "\n} else {\n\t" + apply(els) + "\n}"
                    case None =>
                    "if (" + apply(expr) + ") {\n\t" + apply(thn) + "\n}"
                }
            case While(expr, stat) =>
                "while (" + apply(expr) + ") {\n\t" + apply(stat) + "\t}"
            case Println(expr) =>
                "println(" + apply(expr) + ");"
            case Assign(id, expr) =>
                apply(id) + " = " + apply(expr) + ";"
            case ArrayAssign(id, index, expr) =>
                apply(id) + "[" + apply(index) + "]" + " = " + apply(expr) + ";"
            case Not(expr) =>
                "!(" + apply(expr) + ")"
            case Times(lhs, rhs) =>
                "(" + apply(lhs) + " * " + apply(rhs) + ")"
            case Div(lhs, rhs) =>
                "(" + apply(lhs) + " / " + apply(rhs) + ")"
            case Plus(lhs, rhs) =>
                "(" + apply(lhs) + " + " + apply(rhs) + ")"
            case Minus(lhs, rhs) =>
                "(" + apply(lhs) + " - " + apply(rhs) + ")"
            case LessThan(lhs, rhs) =>
                "(" + apply(lhs) + " < " + apply(rhs) + ")"
            case Equals(lhs, rhs) =>
                "(" + apply(lhs) + " == " + apply(rhs) + ")"
            case And(lhs, rhs) =>
                "(" + apply(lhs) + " && " + apply(rhs) + ")"
            case Or(lhs, rhs) =>
                "(" + apply(lhs) + " || " + apply(rhs) + ")"
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
        }*/
    }

    def run(ctx: Context)(v: Program): String = apply(v)
}
