package toolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      val cf = new ClassFile(ct.id.value, None)
      cf.setSourceFile(ct.id.value + ".toolc")
      cf.addDefaultConstructor

      val ch = cf.addMainMethod.codeHandler

      // TODO 

      ch.freeze

      try {
        cf.writeToFile(dir)
      } catch {
        case io: java.io.IOException =>
          sys.error("Failed to write file!")
      }
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      // TODO
      
      ch.freeze
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
      for (s <- stmts) {
        generateStatCode(ch, s)
      }

      ch << RETURN
      ch.freeze
    }

    def getTypeCode(t: Type): String = t match {
      case TInt => "I"
      case TBoolean => "Z"
      case TIntArray => "A"
      case TString => "Ljava/lang/String;"
    }

    def generateStatCode(ch: CodeHandler, stat: StatTree) {
      stat match {
        case Block(stats) =>
          for (s <- stats) {
            generateStatCode(ch, s)
          }
        case If(expr, thn, els) =>
          val elseLabel = ch.getFreshLabel("else")
          val endLabel = ch.getFreshLabel("end")
          generateExprCode(ch, expr)
          ch << Ldc(0) << If_ICmpEq(elseLabel)
          generateStatCode(ch, thn)
          ch << Goto(endLabel) << Label(elseLabel)
          els match {
            case Some(x) => generateStatCode(ch, x)
            case None =>
          }
          ch << Label(endLabel)
        case While(expr, stats) =>
          val loopLabel = ch.getFreshLabel("loop")
          val endLabel = ch.getFreshLabel("end")
          ch << Label(loopLabel)
          generateExprCode(ch, expr)
          ch << Ldc(0) << If_ICmpEq(endLabel)
          generateStatCode(ch, stats)
          ch << Goto(loopLabel) << Label(endLabel)
        case Println(expr) =>
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          generateExprCode(ch, expr)
          ch << InvokeVirtual("java/io/PrintStream", "println", "(" + getTypeCode(expr.getType) + ")V")
        case Assign(id, expr) =>
          // TODO
        case ArrayAssign(id, index, expr) =>
          // TODO
        case _ => error("Not a statement...")
      }
    }

    def generateExprCode(ch: CodeHandler, e: ExprTree) {
      e match {
        case And(lhs, rhs) =>
          val falseLabel = ch.getFreshLabel("false")
          val endLabel = ch.getFreshLabel("end")
          generateExprCode(ch, lhs)
          ch << Ldc(0) << If_ICmpEq(falseLabel)
          generateExprCode(ch, rhs)
          ch << Ldc(0) << If_ICmpEq(falseLabel) << Ldc(1) << Goto(endLabel) << Label(falseLabel) << Ldc(0) << Label(endLabel)
        case Or(lhs, rhs) =>
          val trueLabel = ch.getFreshLabel("true")
          val endLabel = ch.getFreshLabel("end")
          generateExprCode(ch, lhs)
          ch << Ldc(1) << If_ICmpEq(trueLabel)
          generateExprCode(ch, rhs)
          ch << Ldc(1) << If_ICmpEq(trueLabel) << Ldc(0) << Goto(endLabel) << Label(trueLabel) << Ldc(1) << Label(endLabel)
        case Plus(lhs, rhs) =>
          (lhs.getType, rhs.getType) match {
            case (TInt, TInt) =>
              generateExprCode(ch, lhs)
              generateExprCode(ch, rhs)
              ch << IADD
            case (TInt, TString) =>
              // TODO
            case (TString, TInt) =>
              // TODO
            case (TString, TString) =>
              // TODO
            case _ => error("Unable to generate code for expression: wrong types")
          }
        case Minus(lhs, rhs) =>
          generateExprCode(ch, lhs)
          generateExprCode(ch, rhs)
          ch << ISUB
        case Times(lhs, rhs) =>
          generateExprCode(ch, lhs)
          generateExprCode(ch, rhs)
          ch << IMUL
        case Div(lhs, rhs) =>
          generateExprCode(ch, lhs)
          generateExprCode(ch, rhs)
          ch << IDIV
        case LessThan(lhs, rhs) =>
          val trueLabel = ch.getFreshLabel("true")
          val endLabel = ch.getFreshLabel("end")
          generateExprCode(ch, lhs)
          generateExprCode(ch, rhs)
          ch << If_ICmpLt(trueLabel) << Ldc(0) << Goto(endLabel) << Label(trueLabel) << Ldc(1) << Label(endLabel)
        case Equals(lhs, rhs) =>
          val trueLabel = ch.getFreshLabel("true")
          val endLabel = ch.getFreshLabel("end")
          generateExprCode(ch, lhs)
          generateExprCode(ch, rhs)
          ch << If_ICmpEq(trueLabel) << Ldc(0) << Goto(endLabel) << Label(trueLabel) << Ldc(1) << Label(endLabel)
        case ArrayRead(arr, index) =>
          generateExprCode(ch, arr)
          generateExprCode(ch, index)
          ch << IALOAD
        case ArrayLength(arr) =>
          generateExprCode(ch, arr)
          ch << ARRAYLENGTH
        case MethodCall(obj, meth, args) =>
          // TODO
        case IntLit(value) =>
          ch << Ldc(value)
        case StringLit(value) =>
          ch << Ldc(value)
        case True() => 
          ch << Ldc(1)
        case False() => 
          ch << Ldc(0)
        case Identifier(value) =>
          // TODO
        case This() =>
          ch << ALOAD_0
        case NewIntArray(size) =>
          // TODO
        case New(tpe) =>
          ch << DefaultNew(tpe.value)
        case Not(expr) =>
          generateExprCode(ch, expr)
          ch << Ldc(1) << IXOR
        case _ => error("Not an expression...")
      }
    }

    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    // TODO: Now do the main method
    // ...
  }

}
