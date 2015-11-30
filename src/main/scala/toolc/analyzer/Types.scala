package toolc
package analyzer

import Symbols._

object Types {
  trait Typed {
    self =>

    private var _tpe: Type = TUntyped

    def setType(tpe: Type): self.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _ => false
    }
    override def toString = "int"
  }

  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _ => false
    }
    override def toString = "boolean"
  }

  case object TIntArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TIntArray => true
      case _ => false
    }
    override def toString = "int[]"
  }

  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _ => false
    }
    override def toString = "String"
  }

  case class TObject(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = {
      if (tpe == classSymbol.getType || tpe == TAnyObject) true
      else {
        tpe match {
          case TObject(cs) => 
            cs.parent match {
              case Some(x) =>
                if (x.getType == tpe) true
                else isSubTypeOf(x.getType)
              case None => false
            }
        }
      }
    }
    override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  case object TAnyObject extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TAnyObject => true
      case _ => false
    }
    override def toString = "Object"
  }
}
