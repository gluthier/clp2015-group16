package toolc
package eval

import ast.Trees._
import utils._

class Evaluator(ctx: Context, prog: Program) {

  import ctx.reporter._

  def eval() {
    // Initialize the context for the main method
    val ectx = new MainMethodContext

    // Evaluate each statement of the main method
    prog.main.stats.foreach(evalStatement(ectx, _))
  }

  def evalStatement(ectx: EvaluationContext, stmt: StatTree): Unit = stmt match {
    case Block(stats) =>
      for (s <- stats) evalStatement(ectx, s)

    case If(expr, thn, els) =>
      val cond = evalExpr(ectx, expr).asBool
      if (cond) {
        evalStatement(ectx, thn)
      } else {
        els match {
          case Some(x) => evalStatement(ectx, x)
          case None =>
        }
      }

    case While(expr, stat) =>
      val cond = evalExpr(ectx, expr).asBool
      while (cond) {
        evalStatement(ectx, stat)
      }

    case Println(expr) =>
      val x = evalExpr(ectx, expr)
      x match {
        case IntValue(e) => println(x.asInt)
        case BoolValue(e) => println(x.asBool)
        case StringValue(e) => println(x.asString)
        case _ => fatal("unexpected type")
      }

    case Assign(id, expr) =>
      ectx.setVariable(id.value, evalExpr(ectx, expr))

    case ArrayAssign(id, index, expr) =>
      val array = ectx.getVariable(id.value).asArray
      array.setIndex(evalExpr(ectx, index).asInt, evalExpr(ectx, expr).asInt)

    case _ =>
      fatal("unexpected statement", stmt)
  }

  def evalExpr(ectx: EvaluationContext, e: ExprTree): Value = e match {
    case IntLit(value) => IntValue(value)
    case StringLit(value) => StringValue(value)
    case True() => BoolValue(true)
    case False() => BoolValue(false)

    case And(lhs, rhs) =>
      val lv = evalExpr(ectx, lhs)
      lv match {
        case BoolValue(true) => evalExpr(ectx, rhs)
        case _ => BoolValue(false)
      }

    case Or(lhs, rhs) =>
      val lv = evalExpr(ectx, lhs)
      lv match {
        case BoolValue(true) => BoolValue(true)
        case _ => evalExpr(ectx, rhs)
      }

    case Plus(lhs, rhs) =>
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      (lv, rv) match {
        case (IntValue(l), IntValue(r)) => IntValue(l + r)
        case (StringValue(l), StringValue(r)) => StringValue(l.concat(r))
        case (StringValue(l), IntValue(r)) => StringValue(l.concat(r.toString))
        case (IntValue(l), StringValue(r)) => StringValue(l.toString.concat(r))
        case _ => fatal("unexpected statement")
      }

    case Minus(lhs, rhs) =>
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      (lv, rv) match {
        case (IntValue(l), IntValue(r)) => IntValue(l - r)
        case _ => fatal("unexpected statement")
      }

    case Times(lhs, rhs) =>
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      (lv, rv) match {
        case (IntValue(l), IntValue(r)) => IntValue(l * r)
        case _ => fatal("unexpected statement")
      }

    case Div(lhs, rhs) =>
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      (lv, rv) match {
        case (IntValue(l), IntValue(r)) => IntValue(l / r)
        case _ => fatal("unexpected statement")
      }

    case LessThan(lhs, rhs) =>
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      (lv, rv) match {
        case (IntValue(l), IntValue(r)) => BoolValue(l < r)
        case _ => fatal("unexpected statement")
      }

    case Not(expr) =>
      val e = evalExpr(ectx, expr)
      e match {
        case (BoolValue(b)) => BoolValue(!b)
        case _ => fatal("unexpected statement")
      }

    case Equals(lhs, rhs) =>
      val lv = evalExpr(ectx, lhs)
      val rv = evalExpr(ectx, rhs)
      val res = (lv, rv) match {
        case (IntValue(l), IntValue(r)) => l == r
        case (BoolValue(l), BoolValue(r)) => l == r
        case (lr, rr) => lr eq rr
      }
      BoolValue(res)

    case ArrayRead(arr, index) =>
      val array = evalExpr(ectx, arr)
      val idx = evalExpr(ectx, index)
      IntValue(array.asArray.getIndex(idx.asInt))

    case ArrayLength(arr) =>
      val array = evalExpr(ectx, arr)
      IntValue(array.asArray.size)

    case MethodCall(obj, meth, args) =>
      val objet = evalExpr(ectx, obj)
      val method = new MethodContext(objet.asObject)
      objet match {
        case ObjectValue(x) =>
          val mtc = findMethod(x, meth.value)
          val stats = mtc.stats
          for (i <- mtc.vars) {
            method.declareVariable(x.id.value)
          }
          val test = mtc.args.zip(args)
          for ((a, b) <- test) {
            method.declareVariable(a.id.value)
            method.setVariable(a.id.value, evalExpr(ectx, b))
          }
          for (statement <- stats) {
            evalStatement(method, statement)
          }
          evalExpr(method, mtc.retExpr)
        }
        case _ => fatal("unexpected statement")
      

    case Identifier(name) => ectx.getVariable(name)

    case New(tpe) =>
      val kind = findClass(tpe.value)
      val objet = ObjectValue(kind)
      for (field <- fieldsOfClass(kind)) {
        objet.declareField(field)
      }
      objet

    case This() =>
      ectx match {
        case mc: MethodContext => mc.obj
        case _ => fatal("unexpected statement")
      }

    case NewIntArray(size) =>
      val s = evalExpr(ectx, size).asInt
      new ArrayValue(new Array[Int](s), s)
  }

  // Define the scope of evaluation, with methods to access/declare/set local variables(or arguments)
  abstract class EvaluationContext {
    def getVariable(name: String): Value

    def setVariable(name: String, v: Value): Unit

    def declareVariable(name: String): Unit
  }

  // A Method context consists of the execution context within an object method.
  // getVariable can fallback to the fields of the current object
  class MethodContext(val obj: ObjectValue) extends EvaluationContext {
    var vars = Map[String, Option[Value]]()

    def getVariable(name: String): Value = {
      vars.get(name) match {
        case Some(ov) =>
          ov.getOrElse(fatal("Uninitialized variable '" + name + "'"))

        case _ =>
          obj.getField(name)
      }
    }

    def setVariable(name: String, v: Value) {
      if (vars contains name) {
        vars += name -> Some(v)
      } else {
        obj.setField(name, v)
      }
    }

    def declareVariable(name: String) {
      vars += name -> None
    }
  }

  // Special execution context for the main method, which is very limitted.
  class MainMethodContext extends EvaluationContext {
    def getVariable(name: String): Value = fatal("The main method contains no variable and/or field")

    def setVariable(name: String, v: Value): Unit = fatal("The main method contains no variable and/or field")

    def declareVariable(name: String): Unit = fatal("The main method contains no variable and/or field")
  }

  // Helper functions to query the current program
  def findMethod(cd: ClassDecl, name: String): MethodDecl = {
    cd.methods.find(_.id.value == name).orElse(
      cd.parent.map(p => findMethod(findClass(p.value), name))
    ).getOrElse(fatal("Unknown method " + cd.id + "." + name))
  }

  def findClass(name: String): ClassDecl = {
    prog.classes.find(_.id.value == name).getOrElse(fatal("Unknown class '" + name + "'"))
  }

  def fieldsOfClass(cl: ClassDecl): Set[String] = {
    cl.vars.map(_.id.value).toSet ++
      cl.parent.map(p => fieldsOfClass(findClass(p.value))).getOrElse(Set())
  }

  // Runtime evaluation values, with as* methods which act as typecasts for convenience.
  sealed abstract class Value {
    def asInt: Int = fatal("Unexpected value, found " + this + " expected Int")

    def asString: String = fatal("Unexpected value, found " + this + " expected String")

    def asBool: Boolean = fatal("Unexpected value, found " + this + " expected Boolean")

    def asObject: ObjectValue = fatal("Unexpected value, found " + this + " expected Object")

    def asArray: ArrayValue = fatal("Unexpected value, found " + this + " expected Array")
  }

  case class ObjectValue(cd: ClassDecl) extends Value {
    var fields = Map[String, Option[Value]]()

    def setField(name: String, v: Value) {
      if (fields contains name) {
        fields += name -> Some(v)
      } else {
        fatal("Unknown field '" + name + "'")
      }
    }

    def getField(name: String) = {
      fields.get(name).flatten.getOrElse(fatal("Unknown field '" + name + "'"))
    }

    def declareField(name: String) {
      fields += name -> None
    }

    override def asObject = this
  }

  case class ArrayValue(var entries: Array[Int], val size: Int) extends Value {
    def setIndex(i: Int, v: Int) {
      if (i >= size || i < 0) {
        fatal("Index '" + i + "' out of bounds (0 .. " + size + ")")
      }
      entries(i) = v
    }

    def getIndex(i: Int) = {
      if (i >= size || i < 0) {
        fatal("Index '" + i + "' out of bounds (0 .. " + size + ")")
      }
      entries(i)
    }

    override def asArray = this
  }

  case class StringValue(var v: String) extends Value {
    override def asString = v
  }

  case class IntValue(var v: Int) extends Value {
    override def asInt = v
  }

  case class BoolValue(var v: Boolean) extends Value {
    override def asBool = v
  }

}
