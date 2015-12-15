package toolc
package code

import ast.Trees._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {

  var ids: Map[Identifier, Int] = Map()
  var currentClass: String = "";

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      val cf = new ClassFile(ct.id.value, None)
      cf.setSourceFile(ct.id.value + ".toolc")
      cf.addDefaultConstructor

      for (v <- ct.vars) {
        cf.addField(getTypeCode(v.tpe.getType), v.id.value)
      }

      for (m <- ct.methods) {
        val builder: StringBuilder = new StringBuilder()

        for (v <- m.vars) {
          builder.append(getTypeCode(v.tpe.getType))
        }

        val mh: MethodHandler = cf.addMethod(getTypeCode(m.retType.getType), m.id.value, builder.toString())

        currentClass = ct.id.value
        generateMethodCode(mh.codeHandler, m)
      }

      try {
        cf.writeToFile(dir + "/" + ct.id.value + ".class")
      } catch {
        case io: java.io.IOException =>
          error("Failed to write file!")
      }
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      for (a <- mt.args) {
        ids = ids + (a.id -> ch.getFreshVar)
      }

      for (v <- mt.vars) {
        ids = ids + (v.id -> ch.getFreshVar)
      }

      for (s <- mt.stats) {
        generateStatCode(ch, s)
      }

      generateExprCode(ch, mt.retExpr)

      mt.retType.getType match {
        case TInt => ch << IRETURN
        case TBoolean => ch << IRETURN
        case TIntArray => ch << ARETURN
        case TString => ch << ARETURN
        case _ =>
      }

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
      case TIntArray => "[I"
      case TString => "Ljava/lang/String;"
      case TObject(cs) => "L" + cs.name + ";"
      case _ => ""
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
          if (ids contains id) {
            generateExprCode(ch, expr)
            id.getType match {
              case TInt => IStore(ids(id))
              case TIntArray => AStore(ids(id))
              case TBoolean => IStore(ids(id))
              case TString => AStore(ids(id))
              case _ => AStore(ids(id)) // Object
            }
          } else {
            ch << ALOAD_0
            generateExprCode(ch, expr)
            PutField(currentClass, id.value, getTypeCode(id.getType))
          }
        case ArrayAssign(id, index, expr) =>
          if (ids contains id) {
            ch << ALoad(ids(id))
            generateExprCode(ch, expr)
            generateExprCode(ch, index)
            ch << IASTORE
          } else {
            ch << ALOAD_0
            generateExprCode(ch, expr)
            generateExprCode(ch, index)
            ch << IASTORE
          }
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
              ch << DefaultNew("java/lang/StringBuilder")
              generateExprCode(ch, lhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
              generateExprCode(ch, rhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
            case (TString, TInt) =>
              ch << DefaultNew("java/lang/StringBuilder")
              generateExprCode(ch, lhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
              generateExprCode(ch, rhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
            case (TString, TString) =>
              ch << DefaultNew("java/lang/StringBuilder")
              generateExprCode(ch, lhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
              generateExprCode(ch, rhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
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
          generateExprCode(ch, obj)
          val builder = new StringBuilder()
          builder.append("(")
          for (a <- args) {
            builder.append(getTypeCode(a.getType))
            generateExprCode(ch, a)
          }
          builder.append(")")
          for (c <- prog.classes) {
            for (m <- c.methods) {
              if (m.id == meth)
                builder.append(getTypeCode(m.retType.getType))
            }
          }
          ch << InvokeVirtual(obj.getType.toString, meth.value, builder.toString)
        case IntLit(value) =>
          ch << Ldc(value)
        case StringLit(value) =>
          ch << Ldc(value)
        case True() =>
          ch << Ldc(1)
        case False() =>
          ch << Ldc(0)
        case id: Identifier =>
          ids get id match {
            case Some(x) => ch << ILoad(x)
            case None =>
          }
        case This() =>
          ch << ALOAD_0
        case NewIntArray(size) =>
          generateExprCode(ch, size)
          ch << NewArray(10)
        case New(tpe) =>
          ch << DefaultNew(tpe.value)
        case Not(expr) =>
          generateExprCode(ch, expr)
          ch << Ldc(1) << IXOR
        case _ => error("Not an expression...")
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

    val cf = new ClassFile(prog.main.id.value, None)
    cf.setSourceFile(prog.main.id.value + ".toolc")
    cf.addDefaultConstructor
    val ch = cf.addMainMethod.codeHandler
    generateMainMethodCode(ch, prog.main.stats, prog.main.id.value)

    try {
      cf.writeToFile(outDir + "/" + prog.main.id.value + ".class")
    } catch {
      case io: java.io.IOException =>
        error("Failed to write file!")
    }
  }
}
