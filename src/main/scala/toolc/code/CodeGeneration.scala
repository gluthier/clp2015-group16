package toolc
package code

import ast.Trees._
import analyzer.Symbols._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._
import analyzer.Types._
import collection.mutable

object CodeGeneration extends Pipeline[Program, Unit] {
  def PACKAGE_NAME = ""

  var slotFor: mutable.Map[Int, Int] = mutable.Map()
  var currentClass: String = ""

  def run(ctx: Context)(prog: Program): Unit = {

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {

      val classFile = new ClassFile(ct.id.value, if (ct.parent.isDefined) Some(ct.parent.get.value) else None)
      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor

      ct.vars.foreach { field: VarDecl =>
        classFile.addField(toByteCodeType(field.tpe), field.id.value)
      }

      ct.methods.foreach { method: MethodDecl =>
        val ch: CodeHandler = classFile.addMethod(toByteCodeType(method.retType), method.id.value, method.args.map { arg: Formal =>
          toByteCodeType(arg.tpe)
        }).codeHandler

        currentClass = ct.id.value
        generateMethodCode(ch, method)
      }

      classFile.writeToFile(dir + ct.id.value + ".class")
    }

    def toByteCodeType(tpe: TypeTree): String = {
      tpe match {
        case i: IntType => "I"
        case i: BooleanType => "Z"
        case i: IntArrayType => "[I"
        case i: StringType => "Ljava/lang/String;"
        case Identifier(value: String) => "L" + value + ";"
      }
    }

    def toByteCodeTypes(tpe: Type): String = {
      tpe match {
        case TInt => "I"
        case TBoolean => "Z"
        case TIntArray => "[I"
        case TString => "Ljava/lang/String;"
        case obj: TObject => "L" + obj.classSymbol.name + ";"
        case _ => sys.error("Internal Error: Unknown Type in Code Generation")
      }
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame

    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {

      slotFor.clear()
      for (i <- 1 to mt.args.length) {
        slotFor.put(mt.args(i - 1).getSymbol.id, i)
      }
      mt.vars.foreach { v: VarDecl =>
        slotFor.put(v.getSymbol.id, ch.getFreshVar)
      }

      (mt.stats.map(compileStat(_, ch)) ::: List(compileExpr(mt.retExpr, ch)))
        .foldLeft(ch)((ch, bcg) => ch << bcg)

      ch << {
        mt.retExpr.getType match {
          case TInt => IRETURN
          case TBoolean => IRETURN
          case _ => ARETURN
        }
      }


      if (mt.id.value == "launch") {
        ch.print
      }
      ch.freeze
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {

      stmts.map(compileStat(_, ch))
        .foldLeft(ch)((ch, bcg) => ch << bcg)
      ch << RETURN
      ch.freeze
    }

    def compileStat(stat: StatTree, ch: CodeHandler): AbstractByteCodeGenerator = {
      (ch: CodeHandler) => {
        stat match {

          case Block(stats: List[StatTree]) =>
            stats.foldLeft(ch)((ch, bcg) => ch << compileStat(bcg, ch))

          case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
            val afterLabel: String = ch.getFreshLabel("after")

            if (els.isDefined) {
              val elseLabel: String = ch.getFreshLabel("else")
              ch <<
                compileExpr(expr, ch) <<
                IfEq(elseLabel) <<
                compileStat(thn, ch) <<
                Goto(afterLabel) <<
                Label(elseLabel) <<
                compileStat(els.get, ch) <<
                Label(afterLabel)
            } else {
              ch <<
                compileExpr(expr, ch) <<
                IfEq(afterLabel) <<
                compileStat(thn, ch) <<
                Label(afterLabel)
            }

          case While(expr: ExprTree, stat: StatTree) =>
            val whileLabel = ch.getFreshLabel("while")
            val afterLabel = ch.getFreshLabel("after")

            ch <<
              Label(whileLabel) <<
              compileExpr(expr, ch) <<
              IfEq(afterLabel) <<
              compileStat(stat, ch) <<
              Goto(whileLabel) <<
              Label(afterLabel)

          case Println(expr: ExprTree) =>

            expr.getType match {

              case TString =>
                ch <<
                  GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;") <<
                  compileExpr(expr, ch) <<
                  InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")

              case TInt =>
                ch <<
                  GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;") <<
                  compileExpr(expr, ch) <<
                  InvokeVirtual("java/io/PrintStream", "println", "(I)V")

              case TBoolean =>
                ch <<
                  GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;") <<
                  compileExpr(expr, ch) <<
                  InvokeVirtual("java/io/PrintStream", "println", "(Z)V")

              case _ => sys.error("Internal error: println on wrong type in code generation");
            }

          case Assign(id: Identifier, expr: ExprTree) =>

            // If it's a local variable or argument
            if (slotFor.contains(id.getSymbol.id)) {
              ch <<
                compileExpr(expr, ch) << {
                id.getType match {
                  case TInt => IStore(slotFor(id.getSymbol.id))
                  case TIntArray => AStore(slotFor(id.getSymbol.id))
                  case TBoolean => IStore(slotFor(id.getSymbol.id))
                  case TString => AStore(slotFor(id.getSymbol.id))
                  case _ => AStore(slotFor(id.getSymbol.id)) // Object
                }
              }
            } // If it's a field
            else {
              ch <<
                ALOAD_0 <<
                compileExpr(expr, ch) <<
                PutField(currentClass, id.value, toByteCodeTypes(id.getType))
            }

          case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>

            // For local vars
            if (slotFor.contains(id.getSymbol.id)) {
              ch <<
                ALoad(slotFor(id.getSymbol.id)) <<
                compileExpr(index, ch) <<
                compileExpr(expr, ch) <<
                IASTORE

              // For fields
            } else {
              ch <<
                ALOAD_0 <<
                GetField(currentClass, id.value, "[I") <<
                compileExpr(index, ch) <<
                compileExpr(expr, ch) <<
                IASTORE

            }
        }
      }
    }

    def compileExpr(expr: ExprTree, ch: CodeHandler): AbstractByteCodeGenerator = {
      (ch: CodeHandler) => {
        expr match {
          case And(lhs: ExprTree, rhs: ExprTree) =>
            val elseLabel = ch.getFreshLabel("else")
            val afterLabel = ch.getFreshLabel("after")
            ch <<
              compileExpr(lhs, ch) <<
              IfEq(elseLabel) <<
              compileExpr(rhs, ch) <<
              Goto(afterLabel) <<
              Label(elseLabel) <<
              ICONST_0 <<
              Label(afterLabel)
          case Or(lhs: ExprTree, rhs: ExprTree) =>
            val elseLabel = ch.getFreshLabel("else")
            val afterLabel = ch.getFreshLabel("after")
            ch <<
              compileExpr(lhs, ch) <<
              IfNe(elseLabel) <<
              compileExpr(rhs, ch) <<
              Goto(afterLabel) <<
              Label(elseLabel) <<
              ICONST_1 <<
              Label(afterLabel)

          case Plus(lhs: ExprTree, rhs: ExprTree) =>
            lhs.getType match {
              case TInt => rhs.getType match {
                case TInt =>
                  ch <<
                    compileExpr(lhs, ch) <<
                    compileExpr(rhs, ch) <<
                    IADD
                case TString =>
                  ch <<
                    DefaultNew("java/lang/StringBuilder") <<
                    compileExpr(lhs, ch) <<
                    InvokeVirtual("java/lang/StringBuilder", "append",
                      "(I)Ljava/lang/StringBuilder;") <<
                    compileExpr(rhs, ch) <<
                    InvokeVirtual("java/lang/StringBuilder", "append",
                      "(Ljava/lang/String;)Ljava/lang/StringBuilder;") <<
                    InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
                case _ => sys.error("Internal Error: Wrong type for plus in Code generation")
              }
              case TString => rhs.getType match {
                case TInt =>
                  ch <<
                    DefaultNew("java/lang/StringBuilder") <<
                    compileExpr(lhs, ch) <<
                    InvokeVirtual("java/lang/StringBuilder", "append",
                      "(Ljava/lang/String;)Ljava/lang/StringBuilder;") <<
                    compileExpr(rhs, ch) <<
                    InvokeVirtual("java/lang/StringBuilder", "append",
                      "(I)Ljava/lang/StringBuilder;") <<
                    InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")

                case TString =>
                  ch <<
                    DefaultNew("java/lang/StringBuilder") <<
                    compileExpr(lhs, ch) <<
                    InvokeVirtual("java/lang/StringBuilder", "append",
                      "(Ljava/lang/String;)Ljava/lang/StringBuilder;") <<
                    compileExpr(rhs, ch) <<
                    InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;") <<
                    InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")

                case _ => sys.error("Internal Error: Wrong type for plus in Code generation")
              }

              case _ => sys.error("Internal Error: Wrong type for plus in Code generation")

            }

          case Minus(lhs: ExprTree, rhs: ExprTree) =>
            ch <<
              compileExpr(lhs, ch) <<
              compileExpr(rhs, ch) <<
              ISUB
          case Times(lhs: ExprTree, rhs: ExprTree) =>
            ch <<
              compileExpr(lhs, ch) <<
              compileExpr(rhs, ch) <<
              IMUL
          case Div(lhs: ExprTree, rhs: ExprTree) =>
            ch <<
              compileExpr(lhs, ch) <<
              compileExpr(rhs, ch) <<
              IDIV
          case LessThan(lhs: ExprTree, rhs: ExprTree) =>
            val trueLabel = ch.getFreshLabel("true")
            val afterLabel = ch.getFreshLabel("after")
            ch <<
              compileExpr(lhs, ch) <<
              compileExpr(rhs, ch) <<
              If_ICmpLt(trueLabel) <<
              ICONST_0 <<
              Goto(afterLabel) <<
              Label(trueLabel) <<
              ICONST_1 <<
              Label(afterLabel)
          case Equals(lhs: ExprTree, rhs: ExprTree) =>
            val equalLabel = ch.getFreshLabel("equal")
            val afterLabel = ch.getFreshLabel("after")
            lhs.getType match {
              case TInt =>
                ch <<
                  compileExpr(lhs, ch) <<
                  compileExpr(rhs, ch) <<
                  If_ICmpEq(equalLabel) <<
                  ICONST_0 <<
                  Goto(afterLabel) <<
                  Label(equalLabel) <<
                  ICONST_1 <<
                  Label(afterLabel)

              case TBoolean =>
                ch <<
                  compileExpr(lhs, ch) <<
                  compileExpr(rhs, ch) <<
                  If_ICmpEq(equalLabel) <<
                  ICONST_0 <<
                  Goto(afterLabel) <<
                  Label(equalLabel) <<
                  ICONST_1 <<
                  Label(afterLabel)

              case _ =>
                ch <<
                  compileExpr(lhs, ch) <<
                  compileExpr(rhs, ch) <<
                  If_ACmpEq(equalLabel) <<
                  ICONST_0 <<
                  Goto(afterLabel) <<
                  Label(equalLabel) <<
                  ICONST_1 <<
                  Label(afterLabel)
            }

          case ArrayRead(arr: ExprTree, index: ExprTree) =>
            ch <<
              compileExpr(arr, ch) <<
              compileExpr(index, ch) <<
              IALOAD
          case ArrayLength(arr: ExprTree) =>
            ch <<
              compileExpr(arr, ch) <<
              ARRAYLENGTH
          case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) =>

            val ms = meth.getSymbol.asInstanceOf[MethodSymbol]

            val methodName = meth.value
            val className = ms.classSymbol.name
            val methodSig = "(" + ms.argList.foldLeft(""){(acc, arg) =>
              assert(arg.getType != null)
              acc +
              toByteCodeTypes(arg.getType)} + ")" + toByteCodeTypes(ms.getType)

            val s = List(compileExpr(obj, ch)) ::: args.map(compileExpr(_, ch))
            s.foldLeft(ch)((ch, bcg) => ch << bcg) <<
              InvokeVirtual(className, methodName, methodSig)
          case IntLit(value: Int) =>
            ch << Ldc(value)
          case StringLit(value: String) =>
            ch << Ldc(value)
          case True() =>
            ch << ICONST_1
          case False() =>
            ch << ICONST_0
          case id: Identifier =>

            // If it's a local var or an arg
            if (slotFor.contains(id.getSymbol.id)) {
              id.getType match {
                case TInt =>
                  ch << ILoad(slotFor(id.getSymbol.id))
                case TBoolean =>
                  ch << ILoad(slotFor(id.getSymbol.id))
                case _ =>
                  ch << ALoad(slotFor(id.getSymbol.id))
              }
            } // If it's a field
            else {
              ch <<
                ALOAD_0 <<
                GetField(currentClass, id.value, toByteCodeTypes(id.getType))
            }

          case This() =>
            ch << ALOAD_0
          case NewIntArray(size: ExprTree) =>
            ch <<
              compileExpr(size, ch) <<
              NewArray(NewArray.types("T_INT"))
          case New(tpe: Identifier) =>
            ch << DefaultNew(tpe.value)
          case Not(expr: ExprTree) =>
            val trueLabel = ch.getFreshLabel("true")
            val afterLabel = ch.getFreshLabel("after")
            ch <<
              compileExpr(expr, ch) <<
              IfEq(trueLabel) <<
              ICONST_0 <<
              Goto(afterLabel) <<
              Label(trueLabel) <<
              ICONST_1 <<
              Label(afterLabel)

        }

      }
    }

    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    // Now do the main method
    val cf = new ClassFile(prog.main.id.value, None)
    cf.setSourceFile(sourceName)
    cf.addDefaultConstructor
    val ch = cf.addMainMethod.codeHandler
    generateMainMethodCode(ch, prog.main.stats, prog.main.id.value)
    cf.writeToFile(outDir + prog.main.id.value + ".class")
  }

}