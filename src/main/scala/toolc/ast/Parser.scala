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
        new Program(parseMainObject, parseClassDecl)
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
        // (Statement)*
        val stats = List(parseStatement)
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
        // (VarDeclaration)*
        val vars = List(parseVarDecl)
        // (MethodDeclaration)*
        val methods = List(parseMethodDecl)
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
        // (Identifier: Type(,Identifier:Type)*)?
        val args = currentToken match {
            case IDKIND =>
                val id = parseIdentifier
                eat(COLON)
                val tpe = parseType
                // TODO...
            case _ =>
        }
        eat(RPAREN)
        eat(COLON)
        val tpe = parseType
        eat(EQSIGN)
        eat(LBRACE)
        // (VarDeclaration)*
        val vars = List(parseVarDecl)
        // (Statement)*
        val stats = List(parseStatement)
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
                // ???
                parseIdentifier
        }
    }

    def parseStatement: StatTree = {
        currentToken match {
            case LBRACE =>
                eat(LBRACE)
                // (Statement)*
                val stats = List(parseStatement)
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
            case TRUE =>
                eat(TRUE)
                new True()
            case FALSE =>
                eat(FALSE)
                new False()
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
            case // ???
            case _ => parseIdentifier
        }
    }

    def parseIdentifier: Identifier = {
        currentToken match {
            case IDKIND =>
                // ???
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
