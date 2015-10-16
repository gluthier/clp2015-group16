package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    // Store the current token, as read from the lexer.
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        currentToken = tokens.next

        // skips bad tokens
        while(currentToken.kind == BAD && tokens.hasNext) {
          currentToken = tokens.next
        }
      }
    }

    // ''Eats'' the expected token, or terminates with an error.
    def eat(kind: TokenKind): Unit = {
      if(currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    // Complains that what was found was not expected.
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def parseGoal: Program = {
        val main = parseMainObject
        val classes: List[ClassDecl] = Nil
        while (currentToken.kind != EOF) {
            classes :+ parseClassDecl
        }
        eat(EOF)
        
        new Program(main, classes)
    }

    def parseMainObject: MainObject = {
        eat(OBJECT)
        val id = parseIdentifier
        eat(LBRACE)
        eat(DEF)
        eat(MAIN)
        eat(LPAREN)
        eat(RPAREN)
        eat(SEMICOLON)
        eat(UNIT)
        eat(EQSIGN)
        eat(LBRACE)
        val stats: List[StatTree] = Nil
        while (currentToken.kind != RBRACE) {
            stats :+ parseStatement
        }
        eat(RBRACE)
        eat(RBRACE)

        new MainObject(id, stats)
    }

    def parseClassDecl: ClassDecl = {
        eat(CLASS)
        val id = parseIdentifier
        val parent = currentToken match {
            case EXTENDS =>
                eat(EXTENDS)
                parseIdentifier
            case _ => None
        }
        eat(LBRACE)
        val vars = List[VarDeclaration] = Nil
        while (currentToken.kind == VAR) {
            vars :+ parseVarDecl
        }
        val methods: List[MethodDecl] = Nil
        while (currentToken.kind == DEF) {
            methods :+ parseMethodDecl
        }
        eat(RBRACE)

        new ClassDecl(id, parent, vars, methods)
    }

    def parseVarDecl: VarDeclaration = {
        eat(VAR)
        val id = parseIdentifier
        eat(COLON)
        val tpe = parseType
        eat(SEMICOLON)

        new VarDecl(tpe, id)
    }

    def parseMethodDecl: MethodDecl = {
        eat(DEF)
        val id = parseIdentifier
        eat(LPAREN)
        val args: List[Formal] = Nil
        if (currentToken.kind == IDKIND) {
            val id = parseIdentifier
            eat(COLON)
            val tpe = parseType
            args :+ new Formal(tpe, id)
            while (currentToken.kind == COMMA) {
                eat(COMMA)
                val id = parseIdentifier
                eat(COLON)
                val tpe = parseType
                args :+ new Formal(tpe, id)
            }
        }
        eat(RPAREN)
        eat(COLON)
        val tpe = parseType
        eat(EQSIGN)
        eat(LBRACE)
        val vars: List[VarDeclaration] = Nil
        while (currentToken.kind == VAR) {
            vars :+ parseVarDecl
        }
        val stats: List[StatTree] = Nil
        while (currentToken.kind != RETURN) {
            stats :+ parseStatement
        }
        eat(RETURN)
        val retExpr = parseExpression
        eat(SEMICOLON)
        eat(RBRACE)

        new MethodDecl(retType, id, args, vars, stats, retExpr)
    }

    def parseType: TypeTree = {
        currentToken match {
            case INT =>
                eat(INT)
                currentToken match {
                    case LBRACKET =>
                        eat(LBRACKET)
                        eat(RBRACKET)
                        new IntArrayType()
                    case _ => new IntType()
                }
            case BOOLEAN =>
                eat(BOOLEAN)
                new BooleanType()
            case STRING =>
                eat(STRING)
                new StringType()
            case _ =>
                val value = parseIdentifier
                new Identifier(value)
        }
    }

    def parseStatement: StatTree = {
        currentToken match {
            case LBRACE =>
                eat(LBRACE)
                val stats: List[StatTree] = Nil
                while (currentToken.kind != RBRACE) {
                    stats :+ parseStatement
                }
                eat(RBRACE)
                new Block(stats)
            case IF =>
                eat(IF)
                eat(LPAREN)
                val expr = parseExpression
                eat(RPAREN)
                val thn = parseStatement
                val els = currentToken match {
                    case ELSE =>
                        eat(ELSE)
                        parseStatement
                    case _ => None
                }
                new IF(expr, thn, els)
            case WHILE =>
                eat(WHILE)
                eat(LPAREN)
                val expr = parseExpression
                eat(RPAREN)
                val stat = parseStatement
                new WHILE(expr, stat)
            case PRINTLN =>
                eat(PRINTLN)
                eat(LPAREN)
                val expr = parseExpression
                eat(RPAREN)
                eat(SEMICOLON)
                new Println(expr)
            case _ =>
                val id = parseIdentifier
                currentToken match {
                    case EQSIGN =>
                        eat(EQSIGN)
                        val expr = parseExpression
                        eat(SEMICOLON)
                        new Assign(id, expr)
                    case LBRACKET =>
                        eat(LBRACKET)
                        val index = parseExpression
                        eat(RBRACKET)
                        eat(EQSIGN)
                        val expr = parseExpression
                        eat(SEMICOLON)
                        new ArrayAssign(id, index, expr)
                    case _ => error("error")
                }
        }
    }

    def parseExpression: ExprTree = {
        currentToken match {
            case INTLITKIND =>
                val value = currentToken.value
                eat(INTLITKIND)
                new IntLit(value)
            case STRLITKIND =>
                val value = currentToken.value
                eat(STRLITKIND)
                new StringLit(value)
            case TRUE =>
                eat(TRUE)
                new True()
            case FALSE =>
                eat(FALSE)
                new False()
            case IDKIND =>
                parseIdentifier
            case THIS =>
                eat(THIS)
                new This()
            case NEW =>
                eat(NEW)
                currentToken match {
                    case INT =>
                        eat(INT)
                        eat(LBRACKET)
                        val expr = parseExpression
                        eat(RBRACKET)
                        new NewIntArray(expr)
                    case _ =>
                        val tpe = parseIdentifier
                        eat(LPAREN)
                        eat(RPAREN)
                        new New(tpe)
                }
            case BANG =>
                eat(BANG)
                val expr = parseExpression
                new Not(expr)
            case LPAREN =>
                eat(LPAREN)
                val expr = parseExpression
                eat(RPAREN)
                expr
            case _ =>
                val lhs = parseExpression
                currentToken match {
                    case TIMES =>
                        eat(TIMES)
                        val rhs = parseExpression
                        new Times(lhs, rhs)
                    case DIV =>
                        eat(DIV)
                        val rhs = parseExpression
                        new Div(lhs, rhs)
                    case PLUS =>
                        eat(PLUS)
                        val rhs = parseExpression
                        new Plus(lhs, rhs)
                    case MINUS =>
                        eat(MINUS)
                        val rhs = parseExpression
                        new Minus(lhs, rhs)
                    case LESSTHAN =>
                        eat(LESSTHAN)
                        val rhs = parseExpression
                        new LessThan(lhs, rhs)
                    case EQUALS =>
                        eat(EQUALS)
                        val rhs = parseExpression
                        new Equals(lhs, rhs)
                    case AND =>
                        eat(AND)
                        val rhs = parseExpression
                        new And(lhs, rhs)
                    case OR =>
                        eat(OR)
                        val rhs = parseExpression
                        new Or(lhs, rhs)
                    case LBRACKET =>
                        val arr = lhs
                        eat(LBRACKET)
                        val index = parseExpression
                        eat(RBRACKET)
                        new ArrayRead(arr, index)
                    case DOT =>
                        eat(DOT)
                        currentToken match {
                            case LENGTH =>
                                val arr = lhs
                                eat(LENGTH)
                                new ArrayLength(arr)
                            case IDKIND =>
                                val obj = lhs
                                val meth = parseIdentifier
                                eat(LPAREN)
                                val args: List[ExprTree] = Nil
                                if (currentToken.kind != RPAREN) {
                                    args :+ parseExpression
                                    while (currentToken.kind == COMMA) {
                                        eat(COMMA)
                                        args :+ parseExpression
                                    }
                                }
                                eat(RPAREN)
                                new MethodCall(obj, meth, args)
                        }
                }
        }
    }

    def parseIdentifier: Identifier = {
        currentToken match {
            case IDKIND =>
                val value = currentToken.value
                eat(IDKIND)
                new Identifier(value)
            case _ => erro("error")
        }
    }

    // Initialize the first token
    readToken

    // Parse
    parseGoal
  }
}
