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
        cs.lookupMethod(m.id.value) match {
          case Some(x) =>
            if (x.classSymbol != cs && m.args.size == x.argList.size) {
              createMethods(m, cs, Some(x))
            } else if (x.classSymbol == cs) {
              error("Two methods have the same name!", x)
            } else {
              error("Method " + m.id.value + " is overridden!", x)
            }
          case None =>
            createMethods(m, cs, None)
        }
      }
    }

    for (c <- prog.classes) {
      for (m <- c.methods) {
        for (s <- m.stats) {
          checkStatement(m.getSymbol, s)
        }
      }
    }

    for (s <- prog.main.stats) {
      checkStatement(null, s)
    }


    @tailrec
    def checkParents(checkClass: ClassSymbol, parentClass: ClassSymbol, classList: Set[String]) {
      if (classList contains parentClass.name) {
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
        ms.lookupVar(a.id.value) match {
          case Some(x) =>
            ms.classSymbol.lookupVar(x.name) match {
              case Some(y) =>
                val vs = new VariableSymbol(y.name)
                ms.params += (a.id.value -> vs)
                a.id.setSymbol(vs)
                a.setSymbol(vs)
                vs.setType(retrieveType(a.tpe, gS))
                a.tpe.setType(retrieveType(a.tpe, gS))
              case None =>
                error("Two methods arguments have the same name!", ms)
            }
          case None =>
            val param = new VariableSymbol(a.id.value)
            a.setSymbol(param)
            a.id.setSymbol(param)
            ms.params += a.id.value -> param
            ms.argList = ms.argList :+ param
            param.setType(retrieveType(a.tpe, gS))
            a.tpe.setType(retrieveType(a.tpe, gS))
        }
      }

      ms.overridden = cs.lookupMethod(m.id.value) match {
        case Some(x) =>
          if (cs.name.equals(x.classSymbol.name)) {
            error("Method is overloaded!", ms)
            Some(x)
          } else {
            cs.parent match {
              case Some(p) =>
                if (x.params.size != m.args.size) error("Method is overloaded!", x)
                Some(x)
              case None => error("Method not overridden!", x); None
            }
          }
        case None => None
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
          println("#####" + identifier)
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
              n.tpe.setSymbol(x);
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

  prog

  }

  def retrieveType(t: TypeTree, gS: GlobalScope): Type = {
    t match {
      case IntType() => TInt
      case BooleanType() => TBoolean
      case IntArrayType() => TIntArray
      case StringType() => TString
      case id: Identifier =>
        gS.lookupClass(id.value) match {
          case Some(x) =>
            id.setSymbol(x)
            TObject(x)
          case None => error("Identifier not declared")
        }
    }
  }
}
