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

    def readToken(): Unit = {
      if (tokens.hasNext) {
        currentToken = tokens.next()

        // skips bad tokens
        while(currentToken.kind == BAD && tokens.hasNext) {
          currentToken = tokens.next()
        }
      }
    }

    // ''Eats'' the expected token, or terminates with an error.
    def eat(kind: TokenKind): Unit = {
      if(currentToken.kind == kind) {
        readToken()
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
        var classes: List[ClassDecl] = Nil
        while (currentToken.kind != EOF) {
            classes = parseClassDecl :: classes
        }
        eat(EOF)
        new Program(main, classes.reverse)
    }

    def parseMainObject: MainObject = {
        eat(OBJECT)
        val id = parseIdentifier
        eat(LBRACE)
        eat(DEF)
        eat(MAIN)
        eat(LPAREN)
        eat(RPAREN)
        eat(COLON)
        eat(UNIT)
        eat(EQSIGN)
        eat(LBRACE)
        var stats: List[StatTree] = Nil
        while (currentToken.kind != RBRACE) {
            stats = parseStatement :: stats
        }
        eat(RBRACE)
        eat(RBRACE)
        new MainObject(id, stats.reverse)
    }

    def parseClassDecl: ClassDecl = {
        eat(CLASS)
        val id = parseIdentifier
        val parent = currentToken.kind match {
            case EXTENDS =>
                eat(EXTENDS)
                Some(parseIdentifier)
            case _ => None
        }
        eat(LBRACE)
        var vars: List[VarDecl] = Nil
        while (currentToken.kind == VAR) {
            vars = parseVarDecl :: vars
        }
        var methods: List[MethodDecl] = Nil
        while (currentToken.kind == DEF) {
            methods = parseMethodDecl :: methods
        }
        eat(RBRACE)
        new ClassDecl(id, parent, vars.reverse, methods.reverse)
    }

    def parseVarDecl: VarDecl = {
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
        var args: List[Formal] = Nil
        if (currentToken.kind == IDKIND) {
            val id = parseIdentifier
            eat(COLON)
            val tpe = parseType
            args = new Formal(tpe, id) :: args
            while (currentToken.kind == COMMA) {
                eat(COMMA)
                val id = parseIdentifier
                eat(COLON)
                val tpe = parseType
                args = new Formal(tpe, id) :: args
            }
        }
        eat(RPAREN)
        eat(COLON)
        val retType = parseType
        eat(EQSIGN)
        eat(LBRACE)
        var vars: List[VarDecl] = Nil
        while (currentToken.kind == VAR) {
            vars = parseVarDecl :: vars
        }
        var stats: List[StatTree] = Nil
        while (currentToken.kind != RETURN) {
            stats = parseStatement :: stats
        }
        eat(RETURN)
        val retExpr = parseExpression
        eat(SEMICOLON)
        eat(RBRACE)
        new MethodDecl(retType, id, args.reverse, vars.reverse, stats.reverse, retExpr)
    }

    def parseType: TypeTree = {
        currentToken.kind match {
            case INT =>
                eat(INT)
                currentToken.kind match {
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
                val id = parseIdentifier
                new Identifier(id.value)
        }
    }

    def parseStatement: StatTree = {
        currentToken.kind match {
            case LBRACE =>
                eat(LBRACE)
                var stats: List[StatTree] = Nil
                while (currentToken.kind != RBRACE) {
                    stats = parseStatement :: stats
                }
                eat(RBRACE)
                new Block(stats.reverse)
            case IF =>
                eat(IF)
                eat(LPAREN)
                val expr = parseExpression
                eat(RPAREN)
                val thn = parseStatement
                val els = currentToken.kind match {
                    case ELSE =>
                        eat(ELSE)
                        Some(parseStatement)
                    case _ => None
                }
                new If(expr, thn, els)
            case WHILE =>
                eat(WHILE)
                eat(LPAREN)
                val expr = parseExpression
                eat(RPAREN)
                val stat = parseStatement
                new While(expr, stat)
            case PRINTLN =>
                eat(PRINTLN)
                eat(LPAREN)
                val expr = parseExpression
                eat(RPAREN)
                eat(SEMICOLON)
                new Println(expr)
            case _ =>
                val id = parseIdentifier
                currentToken.kind match {
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
                    case _ => expected(EQSIGN, LBRACKET)
                }
        }
    }

    def parseExpression: ExprTree = {
        def parseExpr(lhsExpr: Option[ExprTree]): ExprTree = {
            lhsExpr match {
                case Some(lhs) =>
                    currentToken.kind match {
                        case TIMES =>
                            eat(TIMES)
                            val rhs = parseExpression
                            parseExpr(Some(new Times(lhs, rhs)))
                        case DIV =>
                            eat(DIV)
                            val rhs = parseExpression
                            parseExpr(Some(new Div(lhs, rhs)))
                        case PLUS =>
                            eat(PLUS)
                            val rhs = parseExpression
                            parseExpr(Some(new Plus(lhs, rhs)))
                        case MINUS =>
                            eat(MINUS)
                            val rhs = parseExpression
                            parseExpr(Some(new Minus(lhs, rhs)))
                        case LESSTHAN =>
                            eat(LESSTHAN)
                            val rhs = parseExpression
                            parseExpr(Some(new LessThan(lhs, rhs)))
                        case EQUALS =>
                            eat(EQUALS)
                            val rhs = parseExpression
                            parseExpr(Some(new Equals(lhs, rhs)))
                        case AND =>
                            eat(AND)
                            val rhs = parseExpression
                            parseExpr(Some(new And(lhs, rhs)))
                        case OR =>
                            eat(OR)
                            val rhs = parseExpression
                            parseExpr(Some(new Or(lhs, rhs)))
                        case LBRACKET =>
                            val arr = lhs
                            eat(LBRACKET)
                            val index = parseExpression
                            eat(RBRACKET)
                            parseExpr(Some(new ArrayRead(arr, index)))
                        case DOT =>
                            eat(DOT)
                            currentToken.kind match {
                                case LENGTH =>
                                    val arr = lhs
                                    eat(LENGTH)
                                    parseExpr(Some(new ArrayLength(arr)))
                                case IDKIND =>
                                    val obj = lhs
                                    val meth = parseIdentifier
                                    eat(LPAREN)
                                    var args: List[ExprTree] = Nil
                                    if (currentToken.kind != RPAREN) {
                                        args = parseExpression :: args
                                        while (currentToken.kind == COMMA) {
                                            eat(COMMA)
                                            args = parseExpression :: args
                                        }
                                    }
                                    eat(RPAREN)
                                    parseExpr(Some(new MethodCall(obj, meth, args.reverse)))
                                case _ => expected(LENGTH, IDKIND)
                            }
                        case _ => lhs //the whole expression has been parsed
                    }
                case None =>
                    currentToken match {
                        case x: INTLIT =>
                            val value = x.value
                            eat(INTLITKIND)
                            parseExpr(Some(new IntLit(value)))
                        case x: STRLIT =>
                            val value = x.value
                            eat(STRLITKIND)
                            parseExpr(Some(new StringLit(value)))
                        case x: ID =>
                            val value = x.value
                            eat(IDKIND)
                            parseExpr(Some(new Identifier(value)))
                        case _ =>
                            currentToken.kind match {
                                case TRUE =>
                                    eat(TRUE)
                                    parseExpr(Some(new True()))
                                case FALSE =>
                                    eat(FALSE)
                                    parseExpr(Some(new False()))
                                case THIS =>
                                    eat(THIS)
                                    parseExpr(Some(new This()))
                                case NEW =>
                                    eat(NEW)
                                    currentToken.kind match {
                                        case INT =>
                                            eat(INT)
                                            eat(LBRACKET)
                                            val expr = parseExpression
                                            eat(RBRACKET)
                                            parseExpr(Some(new NewIntArray(expr)))
                                        case _ =>
                                            val tpe = parseIdentifier
                                            eat(LPAREN)
                                            eat(RPAREN)
                                            parseExpr(Some(new New(tpe)))
                                    }
                                case BANG =>
                                    eat(BANG)
                                    val expr = parseExpression
                                    parseExpr(Some(new Not(expr)))
                                case LPAREN =>
                                    eat(LPAREN)
                                    val expr = parseExpression
                                    eat(RPAREN)
                                    parseExpr(Some(expr))
                                case _ => expected(INTLITKIND, STRLITKIND, IDKIND, TRUE, FALSE, THIS, NEW, BANG, LPAREN)
                            }
                    }
            }
        }

        parseExpr(None)
    }

    def parseIdentifier: Identifier = {
        currentToken match {
            case x: ID =>
                val value = x.value
                eat(IDKIND)
                new Identifier(value)
            case _ => expected(IDKIND)
        }
    }

    // Initialize the first token
    readToken

    // Parse
    parseGoal
  }
}
