package toolc
package analyzer

import utils._
import ast.Trees._
import Symbols._
import Types._

import scala.annotation.tailrec

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    // This is a suggestion:
    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies

    // Make sure you check for all constraints
    val gS = new GlobalScope()

    gS.mainClass = new ClassSymbol(prog.main.id.value)

    prog.main.setSymbol(gS.mainClass)
    prog.main.id.setSymbol(gS.mainClass)
    gS.mainClass.setType(TObject(gS.mainClass))

    /*
     * Checks if:
     *  a class is defined more than once
     *  a class uses the same name as the main object
     */
    for (c <- prog.classes) {
      gS.lookupClass(c.id.value) match {
        case Some(x) => error("Class " + c.id.value + " is defined more than once!", x)
        case None =>
          if (gS.mainClass.name equals c.id.value) {
            error("Class " + c.id.value + " has same name as main object!", gS.mainClass)
          } else {
            val cs = new ClassSymbol(c.id.value)
            c.setSymbol(cs)
            c.id.setSymbol(cs)
            gS.classes += c.id.value -> cs
            cs.setType(TObject(cs))
          }
      }
    }

    /*
     * Checks if:
     *  a variable is defined more than once
     *  the inheritance graph has a cycle
     *  a class name is used as a symbol but is not declared
     */
    for (c <- prog.classes if c.hasSymbol) {
      val cs = c.getSymbol
      c.parent match {
        case Some(p) =>
          cs.parent = gS.lookupClass(p.value) match {
            case Some(e) =>
              val emptySet = Set.empty[String]
              checkParents(cs, e, emptySet)
              p.setSymbol(e)
              Some(e)
            case None =>
              if (p.value equals gS.mainClass.name) {
                error("Class " + c.id.value + "extends the main class!", cs)
                None
              } else {
                error("Class " + c.id.value + " is used as a symbol but is not declared!", cs)
                None
              }
          }
        case None =>
      }

      for (v <- c.vars) {
        cs.lookupVar(v.id.value) match {
          case Some(x) => error("Variable " + v.id.value + " is defined more than once!", cs)
          case None =>
            val vs = new VariableSymbol(v.id.value)
            v.setSymbol(vs)
            v.id.setSymbol(vs)
            vs.setType(retrieveType(v.tpe, gS))
            v.tpe.setType(retrieveType(v.tpe, gS))
            cs.members += v.id.value -> vs
        }
      }

      for (m <- c.methods) {
        cs.methods.get(m.id.value) match {
          case Some(x) =>
            error("Two methods have the same name!", x)
          case None =>
            createMethods(m, cs, None)
        }
      }
    }

    for (c <- prog.classes if c.hasSymbol; m <- c.methods; p <- c.getSymbol.parent) {
      p.lookupMethod(m.id.value) foreach { x =>
        if (x.argList.size != m.getSymbol.argList.size) error("Method invalidly overridden", x)
        else m.getSymbol.overridden = Some(x)
      }


    }

    for (c <- prog.classes) {
      for (m <- c.methods) {
        for (s <- m.stats) {
          checkStatement(m.getSymbol, s)
        }
        checkExpression(m.getSymbol, m.retExpr)
      }
    }

    for (s <- prog.main.stats) {
      checkStatement(null, s)
    }


    @tailrec
    def checkParents(checkClass: ClassSymbol, parentClass: ClassSymbol, classList: Set[String]) {
      if (classList contains parentClass.name || checkClass == parentClass) {
        fatal("There is a cycle in the inheritance!", parentClass)
      } else {
        parentClass.parent match {
          case Some(p) => checkParents(checkClass, p, classList + checkClass.name)
          case None =>
        }
      }
    }

    def createMethods(m: MethodDecl, cs: ClassSymbol, om: Option[MethodSymbol]): Unit = {
      val ms = new MethodSymbol(m.id.value, cs)
      m.setSymbol(ms)
      m.id.setSymbol(ms)
      ms.setType(retrieveType(m.retType, gS))
      m.retType.setType(retrieveType(m.retType, gS))

      for (a <- m.args) {
        ms.params get a.id.value match {
          case Some(x) => error("Already defined", ms)
          case None =>
            val vs = new VariableSymbol(a.id.value)
            ms.params += (a.id.value -> vs)
            ms.argList = ms.argList :+ vs
            a.id.setSymbol(vs)
            a.setSymbol(vs)
            vs.setType(retrieveType(a.tpe, gS))
            a.tpe.setType(retrieveType(a.tpe, gS))
        }
      }

      for (me <- m.vars) {
        ms.params get me.id.value match {
          case Some(x) => error("Member is shadowed!", x)
          case None =>
            val member = new VariableSymbol(me.id.value)
            me.setSymbol(member)
            me.id.setSymbol(member)
            member.setType(retrieveType(me.tpe, gS))
            me.tpe.setType(retrieveType(me.tpe, gS))
            ms.members += me.id.value -> member
        }
      }

      checkExpression(ms, m.retExpr)

      cs.methods += m.id.value -> ms

    }

    def checkStatement(m: MethodSymbol, s: StatTree) {
      s match {
        case block: Block =>
          for (statement <- block.stats) {
            checkStatement(m, statement)
          }
        case print: Println =>
          checkExpression(m, print.expr)
        case Assign(id, expr) =>
          checkExpression(m, id)
          checkExpression(m, expr)
        case ifStat: If =>
          checkExpression(m, ifStat.expr)
          checkStatement(m, ifStat.thn)
          ifStat.els match {
            case Some(e) => checkStatement(m, e)
            case None =>
          }
        case whileStat: While =>
          checkExpression(m, whileStat.expr)
          checkStatement(m, whileStat.stat)
        case array: ArrayAssign =>
          checkExpression(m, array.id)
          checkExpression(m, array.index)
          checkExpression(m, array.expr)
      }
    }

    def checkExpression(s: MethodSymbol, expr: ExprTree) {
      expr match {
        case identifier: Identifier =>
          s lookupVar identifier.value match {
            case Some(v) =>
              identifier.setSymbol(v)
              identifier.setType(v.getType)
            case None =>
              s.classSymbol lookupVar identifier.value match {
                case Some(v) =>
                  identifier.setSymbol(v)
                  identifier.setType(v.getType)
                case None =>
              }
          }
        case and: And =>
          checkExpression(s, and.lhs)
          checkExpression(s, and.rhs)
        case or: Or =>
          checkExpression(s, or.lhs)
          checkExpression(s, or.rhs)
        case plus: Plus =>
          checkExpression(s, plus.lhs)
          checkExpression(s, plus.rhs)
        case minus: Minus =>
          checkExpression(s, minus.lhs)
          checkExpression(s, minus.rhs)
        case times: Times =>
          checkExpression(s, times.lhs)
          checkExpression(s, times.rhs)
        case div: Div =>
          checkExpression(s, div.lhs)
          checkExpression(s, div.rhs)
        case lessThan: LessThan =>
          checkExpression(s, lessThan.lhs)
          checkExpression(s, lessThan.rhs)
        case equals: Equals =>
          checkExpression(s, equals.lhs)
          checkExpression(s, equals.rhs)
        case arrayRead: ArrayRead =>
          checkExpression(s, arrayRead.arr)
          checkExpression(s, arrayRead.index)
        case arrayLength: ArrayLength =>
          checkExpression(s, arrayLength.arr)
        case mc: MethodCall =>
          checkExpression(s, mc.obj)
          for (a <- mc.args) {
            checkExpression(s, a)
          }
        case t: True =>
          t.setType(TBoolean)
        case f: False =>
          f.setType(TBoolean)
        case i: IntLit =>
          i.setType(TInt)
        case s: StringLit =>
          s.setType(TString)
        case t: This =>
          s match {
            case ms: MethodSymbol =>
              t.setSymbol(ms.classSymbol)
              t.setType(TObject(ms.classSymbol))
            case _ =>
          }
        case n: New =>
          gS.lookupClass(n.tpe.value) match {
            case Some(x) =>
              n.tpe.setSymbol(x)
              n.setType(retrieveType(n.tpe, gS))
            case None =>
          }
        case na: NewIntArray =>
          checkExpression(s, na.size)
          na.setType(TIntArray)
        case n: Not =>
          checkExpression(s, n.expr)
        case _ =>
      }
    }

    def retrieveType(t: TypeTree, gS: GlobalScope): Type = {
      t match {
        case IntType() => TInt
        case BooleanType() => TBoolean
        case IntArrayType() => TIntArray
        case StringType() => TString
        case id: Identifier =>
          val symb = gS.lookupClass(id.value).get
          id.setSymbol(symb)
          TObject(symb)
      }
    }

    prog

  }

}
