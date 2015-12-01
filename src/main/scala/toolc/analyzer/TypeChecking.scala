package toolc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {
  /** Typechecking does not produce a new value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case And(lhs, rhs) =>
          (tcExpr(lhs, TBoolean), tcExpr(rhs, TBoolean)) match {
            case (TBoolean, TBoolean) => TBoolean
            case (l, r) => TError
          }
        case Or(lhs, rhs) =>
          (tcExpr(lhs, TBoolean), tcExpr(rhs, TBoolean)) match {
            case (TBoolean, TBoolean) => TBoolean
            case (l, r) => TError
          }
        case Plus(lhs, rhs) =>
          (tcExpr(lhs, TInt, TString), tcExpr(rhs, TInt, TString)) match {
            case (TInt, TInt) => TInt
            case (TInt, TString) => TString
            case (TString, TInt) => TString
            case (TString, TString) => TString
            case (l, r) => TError
          }
        case Minus(lhs, rhs) =>
          (tcExpr(lhs, TInt), tcExpr(rhs, TInt)) match {
            case (TInt, TInt) => TInt
            case _ => TError
          }
        case Times(lhs, rhs) =>
          (tcExpr(lhs, TInt), tcExpr(rhs, TInt)) match {
            case (TInt, TInt) => TInt
            case _ => TError
          }
        case Div(lhs, rhs) =>
          (tcExpr(lhs, TInt), tcExpr(rhs, TInt)) match {
            case (TInt, TInt) => TInt
            case _ => TError
          }
        case LessThan(lhs, rhs) =>
          (tcExpr(lhs, TInt), tcExpr(rhs, TInt)) match {
            case (TInt, TInt) => TBoolean
            case _ => TError
          }
        case Equals(lhs, rhs) =>
          (tcExpr(lhs, TInt, TBoolean, TIntArray, TString), tcExpr(rhs, TInt, TBoolean, TIntArray, TString)) match {
            case (TInt, TInt) => TBoolean
            case (TBoolean, TBoolean) => TBoolean
            case (TIntArray, TIntArray) => TBoolean
            case (TString, TString) => TBoolean
            case (TObject(cs1), TObject(cs2)) => TBoolean
            case _ => TError
          }
        case ArrayRead(arr, index) =>
          tcExpr(arr, TIntArray)
          tcExpr(index, TInt)
          TInt
        case ArrayLength(arr) =>
          tcExpr(arr, TIntArray)
          TInt
        case MethodCall(obj, meth, args) =>
          tcExpr(obj, TAnyObject) match {
            case TObject(cs) =>
              cs.lookupMethod(meth.value) match {
                case Some(x) =>
                  val zip = args zip x.argList.map(_.getType)
                  for (z <- zip) {
                    tcExpr(z._1, z._2)
                  }
                  x.getType
              }
          }
        case IntLit(value) =>
          TInt
        case StringLit(value) =>
          TString
        case True() =>
          TBoolean
        case False() =>
          TBoolean
        case id: Identifier =>
          id.getType
        case This() =>
          expr.getType
        case NewIntArray(size) =>
          tcExpr(size, TInt)
          TIntArray
        case New(tpe) =>
          tpe.getType
        case Not(expr) =>
          tcExpr(expr, TBoolean)
      }

      // Check result and return a valid type in case of error
      if(expected.isEmpty) {
        tpe
      } else {
        if(!expected.exists(e => tpe.isSubTypeOf(e))) {
          error("Type error: Expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
          expected.head
        } else {
          tpe
        }
      }
    }

    def tcStat(stat: StatTree): Unit = {
      stat match {
        case Block(stats) =>
          for (s <- stats) tcStat(s)
        case If(expr, thn, els) =>
          tcExpr(expr, TBoolean)
          tcStat(thn)
          els match {
            case Some(e) => tcStat(e)
            case None =>
          }
        case While(expr, stat) =>
          tcExpr(expr, TBoolean)
          tcStat(stat)
        case Println(expr) =>
          tcExpr(expr, TInt, TBoolean, TString)
        case Assign(id, expr) =>
          tcExpr(expr, id.getType)
        case ArrayAssign(id, index, expr) =>
          tcExpr(index, TInt)
          tcExpr(expr, TInt)
      }
    }
   
    // Traverse and typecheck the program
    // ...
 
    for (s <- prog.main.stats) {
      tcStat(s)
    }

    for (c <- prog.classes) {
      for (m <- c.methods) {
        val mRet = m.retExpr
        val mRetType = m.retType.getType

        tcExpr(mRet, mRetType)

        for (s <- m.stats) {
          tcStat(s)
        }
      }
    }

    prog
  }
}
