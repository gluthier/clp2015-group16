package toolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{ New => _, _ }
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {
  var symToPos: Map[Symbol, Int] = Map()
  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")
    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }
    val sourceName = ctx.file.getName

    // Now do the main method
    val classFile = new ClassFile(prog.main.id.value, None)
    classFile.setSourceFile(sourceName)
    classFile.addDefaultConstructor
    val codeHandler = classFile.addMainMethod.codeHandler
    generateMainMethodCode(codeHandler, prog.main.stats, prog.main.id.value)
    classFile.writeToFile("./" + outDir + "/" + prog.main.id.value + ".class")
    def getClassFileTypeT(tpeT: TypeTree): String = {
      tpeT match {
        case IntArrayType() => "[I"
        case IntType()      => "I"
        case BooleanType()  => "Z"
        case StringType()   => "Ljava/lang/String;"
        case i @ Identifier(_) => "L" + i.value
        case _              => "I"
      }
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      // TODO: Create code handler, save to files ...
      val parentOption = if (ct.parent.isDefined) Some(ct.parent.get.value) else None
      val classFile = new ClassFile(ct.id.value, parentOption)
      classFile.setSourceFile(ct.id.value + ".tool")
      classFile.addDefaultConstructor
      for (v <- ct.vars) {
        val fh: FieldHandler = classFile.addField("toolc/" + v.tpe.getType.toString(), v.id.value)
      }
      for (mt <- ct.methods) {
        var arguList: List[String] = Nil
        for (v <- mt.vars) {
          arguList = arguList.+:(getClassFileType(v.tpe.getType))
        }
        arguList = arguList.reverse

        val mh: MethodHandler = classFile.addMethod(getClassFileTypeT(mt.retType), mt.id.value, arguList: _*)
        generateMethodCode(mh.codeHandler, mt)
      }
      classFile.writeToFile("./" + dir + "/" + ct.id.value + ".class")
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      // TODO: Emit code
      val methSym = mt.getSymbol
      for (arg <- methSym.argList) {
        symToPos += (arg -> ch.getFreshVar)
      }
      for (mem <- methSym.members) {
        symToPos += (mem._2 -> ch.getFreshVar)
      }
      for (s <- mt.stats) {
        statByteCode(s, ch)
      }

      exprByteCode(mt.retExpr, ch)
      mt.retExpr.getType match {
        case TIntArray  => ch << ARETURN
        case TObject(_) => ch << ARETURN
        case TInt       => ch << IRETURN
        case TBoolean   => ch << IRETURN
        case TString    => ch << ARETURN
        case _          => ch << IRETURN
      }

      ch.freeze
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
      // TODO: Emit code
      for (stat <- stmts) {
        statByteCode(stat, ch)
      }
      ch << RETURN
      ch.freeze
    }

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    def getClassFileType(tpe: Type): String = {
      tpe match {
        case TInt        => "I"
        case TBoolean    => "Z"
        case TIntArray   => "[I"
        case TString     => "Ljava/lang/String;"
        case TObject(cs) => "Lpackage/" + cs.name + ";"
        case _           => "I"
      }
    }

    def statByteCode(stat: StatTree, ch: CodeHandler): Unit = stat match {
      case Block(stats: List[StatTree]) => stats.foreach { s => statByteCode(s, ch) }
      case If(expr: ExprTree, thn: StatTree, els: Option[StatTree]) =>
        val fals = ch.getFreshLabel("false")
        val end = ch.getFreshLabel("end")

        ch << Ldc(0) //false (0)
        exprByteCode(expr, ch) //expr
        ch << If_ICmpEq(fals) //expr == false => go to fals
        statByteCode(thn, ch) //expr != false
        ch << Goto(end) //go to end
        ch << Label(fals) //fals :
        if (els.isDefined) statByteCode(els.get, ch)
        ch << Label(end) //end:
      case While(expr: ExprTree, stat: StatTree) =>
        val beginLoop = ch.getFreshLabel("begin_loop")
        val endLoop = ch.getFreshLabel("end_loop")

        ch << Label(beginLoop) //:begin_ loop
        ch << Ldc(0) //false (0)
        exprByteCode(expr, ch) // condition
        ch << If_ICmpEq(endLoop) //condition == false -> endLoop
        statByteCode(stat, ch) //do
        ch << Goto(beginLoop) //loop
        ch << Label(endLoop) //endloop:
      case Println(expr: ExprTree) =>
        ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
        exprByteCode(expr, ch)
        expr.getType match {
          case TInt => ch << InvokeVirtual("java/io/PrintStream", "println", "(I)V")
          case TString => ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
          case TBoolean => ch << InvokeVirtual("java/io/PrintStream", "println", "(Z)V")
          case _ => error("Illegal type for printing")
        }

      case Assign(id: Identifier, expr: ExprTree) =>
        symToPos.get(id.getSymbol) match {
          case Some(i) =>
            exprByteCode(expr, ch)
            ch << IStore(i)
          case None => error("Assigning value to non-defined variable")
        }
      case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) =>
        symToPos.get(id.getSymbol) match {
          case Some(i) =>
            exprByteCode(index, ch)
            exprByteCode(expr, ch)
            ch << AStore(i)
          case None => error("Assigning value to non-defined variable")
        }
    }

    def exprByteCode(expr: ExprTree, ch: CodeHandler): Unit = expr match {
      case And(lhs: ExprTree, rhs: ExprTree) =>
        val fals = ch.getFreshLabel("false")
        val end = ch.getFreshLabel("end")

        exprByteCode(lhs, ch)
        ch << IfEq(fals) // lhs == true then rhs
        ch << Ldc(0) //false
        ch << Goto(end)
        ch << Label(fals)
        exprByteCode(rhs, ch)
        ch << Label(end)

      case Or(lhs: ExprTree, rhs: ExprTree) =>
        val tru = ch.getFreshLabel("true")
        val end = ch.getFreshLabel("end")

        exprByteCode(lhs, ch)
        ch << IfEq(tru) // lhs == true then true
        exprByteCode(rhs, ch)
        ch << Goto(end)
        ch << Label(tru)
        ch << Ldc(1)
        ch << Label(end)

      case Plus(lhs: ExprTree, rhs: ExprTree) => // Attention mélange de types
        (lhs.getType, rhs.getType) match {
          case (TInt, TInt) =>
            exprByteCode(lhs, ch)
            exprByteCode(rhs, ch)
            ch << IADD
          case (TInt, TString)    =>
            ch << DefaultNew("java/lang/StringBuilder")
            exprByteCode(lhs, ch)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
            exprByteCode(rhs, ch)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
            ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
          case (TString, TInt)    =>
            ch << DefaultNew("java/lang/StringBuilder")
            exprByteCode(lhs, ch)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
            exprByteCode(rhs, ch)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
            ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
          case (TString, TString) =>
            ch << DefaultNew("java/lang/StringBuilder")
            exprByteCode(lhs, ch)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
            exprByteCode(rhs, ch)
            ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
            ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
          case _ =>
            error("Addition symbol can be applied with Int and String only")
        }

      case Minus(lhs: ExprTree, rhs: ExprTree) =>
        exprByteCode(lhs, ch)
        exprByteCode(rhs, ch)
        ch << ISUB
      case Times(lhs: ExprTree, rhs: ExprTree) =>
        exprByteCode(lhs, ch)
        exprByteCode(rhs, ch)
        ch << IMUL
      case Div(lhs: ExprTree, rhs: ExprTree) =>
        exprByteCode(lhs, ch)
        exprByteCode(rhs, ch)
        ch << IDIV
      case LessThan(lhs: ExprTree, rhs: ExprTree) =>
        val fals = ch.getFreshLabel("false")
        val end = ch.getFreshLabel("end")
        exprByteCode(lhs, ch)
        exprByteCode(rhs, ch)
        ch << If_ICmpGt(fals)
        ch << Ldc(1)       // true
        ch << Goto(end)    // -> end
        ch << Label(fals)  //fals:
        ch << Ldc(0)       // false
        ch << Label(end)   //end:
      case Equals(lhs: ExprTree, rhs: ExprTree) => // Attention mélange de types
        val fals = ch.getFreshLabel("false")
        val end = ch.getFreshLabel("end")
        exprByteCode(lhs, ch)
        exprByteCode(rhs, ch)
        (lhs.getType, rhs.getType) match {
          case (TInt, TInt) | (TBoolean, TBoolean) =>
            ch << If_ICmpEq(fals)
            ch << Ldc(1)          //true
            ch << Goto(end)       //end:
            ch << Label(fals)     //fals:
            ch << Ldc(0)          // false
            ch << Label(end)      //end:
          case (TIntArray, TIntArray) | (TString, TString) |  (TObject(_), TObject(_)) =>
            ch << If_ACmpEq(fals)
            ch << Ldc(1)          //true
            ch << Goto(end)       //end:
            ch << Label(fals)     //fals:
            ch << Ldc(0)          // false
            ch << Label(end)      //end:
          case (_, _) => error("EQUALS : Type mismatch"); ch << POP << POP
        }
      case ArrayRead(arr: ExprTree, index: ExprTree) =>
        exprByteCode(arr, ch)
        exprByteCode(index, ch)
        ch << IALOAD
      case ArrayLength(arr: ExprTree) =>
        exprByteCode(arr, ch)
        ch << ARRAYLENGTH
      case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) =>
        exprByteCode(obj, ch)
        var str : String = "("

        val ms = meth.getSymbol match {
          case ms : ClassSymbol => ms.lookupMethod(meth.value).get;
          case ms : MethodSymbol => ms
          case _ => fatal("Cannot get symbol from method identifier", meth)
        }

        for (a <- ms.argList) {
          str += getClassFileType(a.getType)
        }
        str += ")" + getClassFileType(ms.getType)
        ch << InvokeVirtual(obj.getType.toString(), meth.value, str)
      case IntLit(value: Int) =>
        ch << Ldc(value)
      case StringLit(value: String) =>
        ch << Ldc(value)
      case True() =>
        ch << Ldc(1)
      case False() =>
        ch << Ldc(0)
      case a @ Identifier(value: String)   => a.getType match {
        case TInt | TBoolean =>
          if (symToPos.get(a.getSymbol).isDefined) {
            ch << ILoad(symToPos.get(a.getSymbol).get)
          } else error("Identifier not intialized")
        case TIntArray | TString | TObject(_) =>
          if (symToPos.get(a.getSymbol).isDefined) {
            ch << ALoad(symToPos.get(a.getSymbol).get)
          } else error("Identifier not intialized")
        case _  => error("Unknown Type")
      }
      case This()  => ch << ALOAD_0
      case NewIntArray(size: ExprTree) =>
        exprByteCode(size, ch)
        ch << NewArray("I")
      case New(tpe: Identifier)        => ch << DefaultNew(tpe.value)
      case Not(expr: ExprTree) =>
        val fals = ch.getFreshLabel("false")
        val end = ch.getFreshLabel("end")
        exprByteCode(expr, ch)
        ch << Ldc(0)  //false
        ch << If_ICmpEq(fals)
        ch << Ldc(0)
        ch << Goto(end)
        ch << Label(fals) //fals:
        ch << Ldc(1)
        ch << Label(end)
    }
  }
}
