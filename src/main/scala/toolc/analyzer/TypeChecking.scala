package toolc
package analyzer

import ast.Trees._

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
            case (l, r) => 
              error("Type error: Expected: " + expected.toList.mkString(" or ") + ", found: " + lhs.getType + ", " + rhs.getType)
              TError
          }
        case Or(lhs, rhs) =>
          (tcExpr(lhs, TBoolean), tcExpr(rhs, TBoolean)) match {
            case (TBoolean, TBoolean) => TBoolean
            case (l, r) => 
              error("Type error: Expected: " + expected.toList.mkString(" or ") + ", found: " + lhs.getType + ", " + rhs.getType)
              TError
          }
        case Plus(lhs, rhs) =>
          (tcExpr(lhs, TInt, TString), tcExpr(rhs, TInt, TString)) match {
            case (TInt, TInt) => TInt
            case (TInt, TString) => TString
            case (TString, TInt) => TString
            case (TString, TString) => TString
            case (l, r) => 
              error("Type error: Expected: " + expected.toList.mkString(" or ") + ", found: " + lhs.getType + ", " + rhs.getType)
              TError
          }
        case Minus(lhs, rhs) =>
          (tcExpr(lhs, TInt), tcExpr(rhs, TInt)) match {
            case (TInt, TInt) => TInt
            case _ => 
              error("Type error: Expected: " + expected.toList.mkString(" or ") + ", found: " + lhs.getType + ", " + rhs.getType)
              TError
          }
        case Times(lhs, rhs) =>
          (tcExpr(lhs, TInt), tcExpr(rhs, TInt)) match {
            case (TInt, TInt) => TInt
            case _ => 
              error("Type error: Expected: " + expected.toList.mkString(" or ") + ", found: " + lhs.getType + ", " + rhs.getType)
              TError
          }
        case Div(lhs, rhs) =>
          (tcExpr(lhs, TInt), tcExpr(rhs, TInt)) match {
            case (TInt, TInt) => TInt
            case _ => 
              error("Type error: Expected: " + expected.toList.mkString(" or ") + ", found: " + lhs.getType + ", " + rhs.getType)
              TError
          }
        case LessThan(lhs, rhs) =>
          (tcExpr(lhs, TInt), tcExpr(rhs, TInt)) match {
            case (TInt, TInt) => TBoolean
            case _ =>
              error("Type error: Expected: " + expected.toList.mkString(" or ") + ", found: " + lhs.getType + ", " + rhs.getType)
              TError
          }
        case Equals(lhs, rhs) =>
          (tcExpr(lhs, TInt, TBoolean, TIntArray, TString, TAnyObject), tcExpr(rhs, TInt, TBoolean, TIntArray, TString, TAnyObject)) match {
            case (TInt, TInt) => TBoolean
            case (TBoolean, TBoolean) => TBoolean
            case (TIntArray, TIntArray) => TBoolean
            case (TString, TString) => TBoolean
            case (TObject(_), TObject(_)) => TBoolean
            case _ => 
              error("Type error: Expected: " + expected.toList.mkString(" or ") + ", found: " + lhs.getType + ", " + rhs.getType)
              TError
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
        case New(tpee) =>
          tpee.getType
        case Not(expre) =>
          tcExpr(expre, TBoolean)
      }

      // Set the type on the expression tree
      expr.setType(tpe)

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
        case While(expr, stats) =>
          tcExpr(expr, TBoolean)
          tcStat(stats)
        case Println(expr) =>
          tcExpr(expr, TInt, TBoolean, TString)
        case Assign(id, expr) =>
          tcExpr(expr, id.getType)
        case ArrayAssign(id, index, expr) =>
          tcExpr(id, TIntArray)
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

        val mSymbol = m.getSymbol
        mSymbol.overridden match {
          case Some(x) =>
            if (mRetType != x.getType)
              error("Overridden method's return type does not match: Expected: " + x.getType + ", found: " + mRetType)
            if (m.args.size != x.argList.size)
              error("Overridden method's number of arguments does not match: Expected: " + x.argList.size + ", found: " + m.args.size)
          case None =>
        }

        tcExpr(mRet, mRetType)

        for (s <- m.stats) {
          tcStat(s)
        }
      }
    }

    prog
  }
}
