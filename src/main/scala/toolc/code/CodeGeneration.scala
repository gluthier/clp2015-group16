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

    // global map representing the slots for the local variables
    var localVarSlots: List[(String, Int)] = Nil

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      val classFile: ClassFile = if (ct.parent.isDefined) new ClassFile(ct.id.value, Some(ct.parent.get.value))
      else new ClassFile(ct.id.value, None)

      // Set source file
      classFile.setSourceFile(sourceName)

      //Add default constructor
      classFile.addDefaultConstructor

      // Add fields
      ct.vars foreach {
        field => classFile.addField(JVMTypeForTypeTree(field.tpe), field.id.value)
      }

      //Add methods
      ct.methods foreach {
        method => {
          val argTypes : List[TypeTree] = method.args.map( formal => formal.tpe)
          val mh:MethodHandler = classFile.addMethod(JVMTypeForTypeTree(method.retType), method.id.value, listJVMTypesII(argTypes))
          generateMethodCode(mh.codeHandler, method)
        }
      }

      // Write .class
      classFile.writeToFile(dir + ct.id.value + ".class")
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      // Create the slotFor map for local variables
      //var localVarSlots: List[(String, Int)] = Nil
      localVarSlots = Nil
      localVarSlots = localVarSlots ++ assignSlotsForArgs(mt.args, 1)

      // Declare local variables
      localVarSlots = localVarSlots ++ mt.vars.map(assignSlotForVarDecl(_,ch))

      // Emit code for statements
      mt.stats foreach {
        stat => generateStatementCode(ch, stat, methSym)
      }

      // Emit code for return expression (return expression left in top of stack)
      generateExpressionCode(ch, mt.retExpr, methSym)

      // Emit code to return type
      mt.retType match {
        case IntType() => ch << IRETURN
        case BooleanType() => ch << IRETURN
        case default => ch << ARETURN
      }

      ch.freeze
    }

    def assignSlotsForArgs(args: List[Formal], slot: Int): List[(String, Int)] = {
      if(args.isEmpty) Nil
      else List((args.head.id.value, slot)) ++ assignSlotsForArgs(args.tail, slot + 1)
    }

    def assignSlotForVarDecl(variable: VarDecl, ch: CodeHandler): (String, Int) = {
      (variable.id.value, ch.getFreshVar)
    }

    // Returns the Int corresponding to the slot assigned to the local variable or None if the variable is global (field)
    def slotFor(varName: String): Option[Int] = {
      slotSearch(localVarSlots, varName)
    }

    def slotSearch(slots: List[(String, Int)], varName: String): Option[Int] = {
      if(slots.isEmpty) None
      else
      if( slots.head._1 == varName ) Some(slots.head._2)
      else slotSearch(slots.tail, varName)
    }

    def generateStatementCode(ch: CodeHandler, st: StatTree, ms: MethodSymbol ): Unit = {
      st match {
        case stat: Block => {
          stat.stats.foreach(generateStatementCode(ch, _, ms))
        }
        case stat: If => {
          val nElse = ch.getFreshLabel("nElse")
          val nAfter = ch.getFreshLabel("nAfter")
          generateExpressionCode(ch, stat.expr, ms)
          ch << IfEq(nElse)
          generateStatementCode(ch, stat.thn, ms)
          ch << Goto(nAfter)
          ch << Label(nElse)
          if(stat.els.isDefined){
            generateStatementCode(ch, stat.els.get, ms)
          }
          ch << Label(nAfter)
        }
        case stat: While => {
          val nStart = ch.getFreshLabel("nStart")
          val nAfter = ch.getFreshLabel("nAfter")
          ch << Label(nStart)
          generateExpressionCode(ch, stat.expr, ms)
          ch << IfEq(nAfter)
          generateStatementCode(ch, stat.stat, ms)
          ch<< Goto(nStart)
          ch<<Label(nAfter)
        }
        case stat: Println => {
          ch << GetStatic("java/lang/System","out","Ljava/io/PrintStream;")
          generateExpressionCode(ch, stat.expr, ms)
          ch << InvokeVirtual("java/io/PrintStream", "println", "("+JVMType(stat.expr.getType)+")V")
        }
        case stat: Assign => {
          if(slotFor(stat.id.value).isDefined){ // Assignment to local variable
            generateExpressionCode(ch, stat.expr, ms)
            stat.id.getType match {
              case TInt | TBoolean => ch << IStore(slotFor(stat.id.value).get)
              case default => ch << AStore(slotFor(stat.id.value).get)
            }
          }
          else{ // Assignment to global field
            ch << ALoad(0)
            generateExpressionCode(ch, stat.expr, ms)
            ch << PutField(ms.classSymbol.name, stat.id.value, JVMType(stat.id.getType))
          }
        }
        case stat: ArrayAssign => {
          // Load array
          if(slotFor(stat.id.value).isDefined){//Local array
            ch << ALoad(slotFor(stat.id.value).get)
          }
          else{
            ch << ALoad(0)
            ch << GetField(ms.classSymbol.name, stat.id.value, JVMType(stat.id.getType))
          }
          // Set parameters
          generateExpressionCode(ch, stat.index, ms)
          generateExpressionCode(ch, stat.expr, ms)

          // Store new int in array
          ch << IASTORE
        }
      }
    }

    def generateExpressionCode(ch: CodeHandler, ex: ExprTree, ms: MethodSymbol): Unit = {
      ex match{
        case expr: And => {
          val nElse = ch.getFreshLabel("nElse")
          val nAfter = ch.getFreshLabel("nAfter")
          generateExpressionCode(ch, expr.lhs, ms)
          ch << IfEq(nElse)
          generateExpressionCode(ch, expr.rhs, ms)
          ch << Goto(nAfter)
          ch << Label(nElse)
          ch << Ldc(0)
          ch << Label(nAfter)
        }
        case expr: Or => {
          val nElse = ch.getFreshLabel("nElse")
          val nAfter = ch.getFreshLabel("nAfter")
          generateExpressionCode(ch, expr.lhs, ms)
          ch << IfEq(nElse)
          ch << Ldc(1)
          ch << Goto(nAfter)
          ch << Label(nElse)
          generateExpressionCode(ch, expr.rhs, ms)
          ch << Label(nAfter)
        }
        case expr: Equals => {
          val nTrue = ch.getFreshLabel("nTrue")
          val nAfter = ch.getFreshLabel("nAfter")
          generateExpressionCode(ch, expr.lhs, ms)
          generateExpressionCode(ch, expr.rhs, ms)
          expr.lhs.getType match {
            case TInt | TBoolean => ch << If_ICmpEq(nTrue)
            case default => ch << If_ACmpEq(nTrue)
          }
          ch << Ldc(0)
          ch << Goto(nAfter)
          ch << Label(nTrue)
          ch << Ldc(1)
          ch << Label(nAfter)
        }
        case expr: LessThan => {
          val nTrue = ch.getFreshLabel("nTrue")
          val nAfter = ch.getFreshLabel("nAfter")
          generateExpressionCode(ch, expr.lhs, ms)
          generateExpressionCode(ch, expr.rhs, ms)
          ch << If_ICmpLt(nTrue)
          ch << Ldc(0)
          ch << Goto(nAfter)
          ch << Label(nTrue)
          ch << Ldc(1)
          ch << Label(nAfter)
        }
        case expr: Plus => {
          expr.getType match {
            case TInt => { // Numerical addition
              generateExpressionCode(ch, expr.lhs, ms)
              generateExpressionCode(ch, expr.rhs, ms)
              ch << IADD
            }
            case TString => { // Append two strings or string, int
              ch << DefaultNew("java/lang/StringBuilder")
              generateExpressionCode(ch, expr.lhs, ms)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + JVMType(expr.lhs.getType) + ")Ljava/lang/StringBuilder;")
              generateExpressionCode(ch, expr.rhs, ms)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", "(" + JVMType(expr.rhs.getType) + ")Ljava/lang/StringBuilder;")
              ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
            }
            case default => //Unreachable thanks to type checking
          }
        }
        case expr: Minus => {
          generateExpressionCode(ch, expr.lhs, ms)
          generateExpressionCode(ch, expr.rhs, ms)
          ch << ISUB
        }
        case expr: Times => {
          generateExpressionCode(ch, expr.lhs, ms)
          generateExpressionCode(ch, expr.rhs, ms)
          ch << IMUL
        }
        case expr: Div => {
          generateExpressionCode(ch, expr.lhs, ms)
          generateExpressionCode(ch, expr.rhs, ms)
          ch << IDIV
        }
        case expr: ArrayRead =>{
          generateExpressionCode(ch, expr.arr, ms)
          generateExpressionCode(ch, expr.index, ms)
          ch << IALOAD
        }
        case expr: ArrayLength => {
          generateExpressionCode(ch, expr.arr, ms)
          ch << ARRAYLENGTH
        }
        case expr: MethodCall => {// Special consideration to identifier
          generateExpressionCode(ch, expr.obj, ms)
          expr.args.foreach(generateExpressionCode(ch, _, ms))
          expr.meth.getSymbol match {
            case msCall : MethodSymbol => {
              val argTypes: List[Type] = msCall.argList.map(x => x.getType)
              ch << InvokeVirtual(msCall.classSymbol.name , msCall.name , "(" + listJVMTypes(argTypes) + ")" + JVMType(msCall.getType))
            }
            case default => //Unreachable code thanks to Type checking
          }

        }
        case expr: IntLit => ch << Ldc(expr.value)
        case expr: StringLit => ch << Ldc(expr.value)
        case expr: True => ch << Ldc(1)
        case expr: False => ch << Ldc(0)
        case expr: Identifier => { // This can only be a variable
          if(slotFor(expr.value).isDefined){//Local variable
            expr.getType match {
              case TInt | TBoolean => ch << ILoad(slotFor(expr.value).get)
              case default => ch << ALoad(slotFor(expr.value).get)
            }
          }
          else{ // Global variable (field)
            ch << ALoad(0)
            ch << GetField(ms.classSymbol.name, expr.value, JVMType(expr.getType))
          }
        }
        case expr: This => ch << ALoad(0)
        case expr: NewIntArray => {
          generateExpressionCode(ch, expr.size, ms)
          ch << NewArray(10)
        }
        case expr: New => {
          ch << DefaultNew(expr.tpe.value)
        }
        case expr: Not => {
          val nElse = ch.getFreshLabel("nElse")
          val nAfter = ch.getFreshLabel("nAfter")
          generateExpressionCode(ch, expr.expr, ms)
          ch << IfEq(nElse)
          ch << Ldc(0)
          ch << Goto(nAfter)
          ch << Label(nElse)
          ch << Ldc(1)
          ch << Label(nAfter)
        }
      }
    }

    def generateMainMethodCode(ch: CodeHandler, stmts: List[StatTree], cname: String): Unit = {
      // Emit code for statements
      stmts foreach {
        stat => generateStatementCode(ch, stat, null)
      }

      ch << RETURN
      ch.freeze
    }

    def JVMTypeForTypeTree(tpe: TypeTree): String = tpe match {
      case IntArrayType() => "[I"
      case IntType() => "I"
      case BooleanType() => "Z"
      case StringType() => "Ljava/lang/String;"
      case id: Identifier => "L"+ id.value + ";"  // Object Reference
    }

    def JVMType(tpe: Type): String = tpe match {
      case TIntArray => "[I"
      case TInt => "I"
      case TBoolean => "Z"
      case TString => "Ljava/lang/String;"
      case id: TObject => "L"+ id.toString + ";"  // Object Reference
      case default => {
        java.lang.System.err.println("Error at code generation: Unassigned type") //TError, TUntyped
        //System.exit(-1)
        "ERROR" // Unreachable code
      }
    }

    def listJVMTypes(args: List[Type]): String = {
      val types = args.map(JVMType)
      types.mkString("")
    }

    def listJVMTypesII(args: List[TypeTree]): String = {
      val types = args.map(JVMTypeForTypeTree)
      types.mkString("")
    }

    def generateMainClassFile(sourceName: String, main: MainObject, dir: String): Unit = {
      val classFile: ClassFile =  new ClassFile(main.id.value, None)

      // Set source file
      classFile.setSourceFile(sourceName)

      //Add default constructor
      classFile.addDefaultConstructor

      //Add main method
      val mh: MethodHandler = classFile.addMainMethod
      generateMainMethodCode(mh.codeHandler, main.stats, main.id.value)

      // Write .class
      classFile.writeToFile(dir + main.id.value + ".class")
    }

    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.getName

    generateMainClassFile(sourceName, prog.main ,outDir)

    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

  }

}