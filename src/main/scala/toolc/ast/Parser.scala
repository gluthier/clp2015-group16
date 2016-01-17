package toolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._
import analyzer.Types._

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
		if (currentToken.kind == SEMICOLON) {
        	eat(SEMICOLON)
		}
        new VarDecl(tpe, id)
    }

    def parseMethodDecl: MethodDecl = {
        eat(DEF)
		val id = currentToken.kind match {
			case OR =>
				eat(OR)
				new Identifier("||")
			case AND =>
				eat(AND)
				new Identifier("&&")
			case LESSTHAN =>
				eat(LESSTHAN)
				new Identifier("<")
			case EQUALS =>
				eat(EQUALS)
				new Identifier("==")
			case PLUS =>
				eat(PLUS)
				new Identifier("+")
			case MINUS =>
				eat(MINUS)
				new Identifier("-")
			case TIMES =>
				eat(TIMES)
				new Identifier("*")
			case DIV =>
				eat(DIV)
				new Identifier("/")
			case BANG =>
				eat(BANG)
				new Identifier("!")
			case _ =>
				parseIdentifier
		}
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
		if (currentToken.kind == SEMICOLON) {
        	eat(SEMICOLON)
		}
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
				if (currentToken.kind == SEMICOLON) {
		        	eat(SEMICOLON)
				}
                new Println(expr)
            case _ =>
                val id = parseIdentifier
                currentToken.kind match {
                    case EQSIGN =>
                        eat(EQSIGN)
                        val expr = parseExpression
						if (currentToken.kind == SEMICOLON) {
				        	eat(SEMICOLON)
						}
                        new Assign(id, expr)
                    case LBRACKET =>
                        eat(LBRACKET)
                        val index = parseExpression
                        eat(RBRACKET)
                        eat(EQSIGN)
                        val expr = parseExpression
						if (currentToken.kind == SEMICOLON) {
				        	eat(SEMICOLON)
						}
                        new ArrayAssign(id, index, expr)
                    case _ => expected(EQSIGN, LBRACKET)
                }
        }
    }

    def parseExpression: ExprTree = parseNext(parseOrExpr)

    def parseOr(lhs: ExprTree): ExprTree = {
        currentToken.kind match {
            case OR =>
                eat(OR)
				lhs.getType match {
					case TObject(cs) =>
						cs.lookupMethod("||") match {
							case Some(ms) =>
								parseOr(new MethodCall(lhs, new Identifier("||"), List(parseAndExpr)))
							case None =>
								fatal("|| method is not defined for Object " + lhs.toString)
						}
					case _ =>
						parseOr(new Or(lhs, parseAndExpr))
				}
            case _ => lhs
        }
    }

    def parseOrExpr: ExprTree = parseNext(parseOr(parseAndExpr))

    def parseAnd(lhs: ExprTree): ExprTree = {
        currentToken.kind match {
            case AND =>
                eat(AND)
				lhs.getType match {
					case TObject(cs) =>
						cs.lookupMethod("&&") match {
							case Some(ms) =>
								parseAnd(new MethodCall(lhs, new Identifier("&&"), List(parseLessThanEqualsExpr)))
							case None =>
								fatal("&& method is not defined for Object " + lhs.toString)
						}
					case _ =>
						parseAnd(new And(lhs, parseLessThanEqualsExpr))
				}
            case _ => lhs
        }
    }

    def parseAndExpr: ExprTree = parseNext(parseAnd(parseLessThanEqualsExpr))

    def parseLessThanEquals(lhs: ExprTree): ExprTree = {
        currentToken.kind match {
            case LESSTHAN =>
                eat(LESSTHAN)
				lhs.getType match {
					case TObject(cs) =>
						cs.lookupMethod("<") match {
							case Some(ms) =>
								parseLessThanEquals(new MethodCall(lhs, new Identifier("<"), List(parsePlusMinusExpr)))
							case None =>
								fatal("< method is not defined for Object " + lhs.toString)
						}
					case _ =>
						parseLessThanEquals(new LessThan(lhs, parsePlusMinusExpr))
				}
            case EQUALS =>
                eat(EQUALS)
				lhs.getType match {
					case TObject(cs) =>
						cs.lookupMethod("==") match {
							case Some(ms) =>
								parseLessThanEquals(new MethodCall(lhs, new Identifier("=="), List(parsePlusMinusExpr)))
							case None =>
								fatal("== method is not defined for Object " + lhs.toString)
						}
					case _ =>
						parseLessThanEquals(new Equals(lhs, parsePlusMinusExpr))
				}
            case _ => lhs
        }
    }

    def parseLessThanEqualsExpr: ExprTree = parseNext(parseLessThanEquals(parsePlusMinusExpr))

    def parsePlusMinus(lhs: ExprTree): ExprTree = {
        currentToken.kind match {
            case PLUS =>
                eat(PLUS)
				lhs.getType match {
					case TObject(cs) =>
						cs.lookupMethod("+") match {
							case Some(ms) =>
								parsePlusMinus(new MethodCall(lhs, new Identifier("+"), List(parseTimesDivExpr)))
							case None =>
								fatal("+ method is not defined for Object " + lhs.toString)
						}
					case _ =>
						parsePlusMinus(new Plus(lhs, parseTimesDivExpr))
				}
            case MINUS =>
                eat(MINUS)
				lhs.getType match {
					case TObject(cs) =>
						cs.lookupMethod("-") match {
							case Some(ms) =>
								parsePlusMinus(new MethodCall(lhs, new Identifier("-"), List(parseTimesDivExpr)))
							case None =>
								fatal("- method is not defined for Object " + lhs.toString)
						}
					case _ =>
						parsePlusMinus(new Minus(lhs, parseTimesDivExpr))
				}
            case _ => lhs
        }
    }

    def parsePlusMinusExpr: ExprTree = parseNext(parsePlusMinus(parseTimesDivExpr))

    def parseTimesDiv(lhs: ExprTree): ExprTree = {
        currentToken.kind match {
            case TIMES =>
                eat(TIMES)
				lhs.getType match {
					case TObject(cs) =>
						cs.lookupMethod("*") match {
							case Some(ms) =>
								parseTimesDiv(new MethodCall(lhs, new Identifier("*"), List(parseBangExpr)))
							case None =>
								fatal("* method is not defined for Object " + lhs.toString)
						}
					case _ =>
						parseTimesDiv(new Times(lhs, parseBangExpr))
				}
            case DIV =>
                eat(DIV)
				lhs.getType match {
					case TObject(cs) =>
						cs.lookupMethod("/") match {
							case Some(ms) =>
								parseTimesDiv(new MethodCall(lhs, new Identifier("/"), List(parseBangExpr)))
							case None =>
								fatal("/ method is not defined for Object " + lhs.toString)
						}
					case _ =>
						parseTimesDiv(new Div(lhs, parseBangExpr))
				}
            case _ => lhs
        }
    }

    def parseTimesDivExpr: ExprTree = parseNext(parseTimesDiv(parseBang))

    def parseBang: ExprTree = {
        currentToken.kind match {
            case BANG =>
                eat(BANG)
				val nextExpr = parseNextExpr
				nextExpr.getType match {
					case TObject(cs) =>
						cs.lookupMethod("!") match {
							case Some(ms) =>
								parseNext(new MethodCall(nextExpr, new Identifier("!"), List(parseNextExpr)))
							case None =>
								fatal("! method is not defined for Object " + nextExpr.toString)
						}
					case _ =>
						parseNext(new Not(nextExpr))
				}
            case _ => parseBangExpr
        }
    }

    def parseBangExpr: ExprTree = parseNext(parseSimpleExpr)

	def parseArgumentsMethod: List[ExprTree] = {
		var args: List[ExprTree] = Nil
		if (currentToken.kind != RPAREN) {
			args = parseExpression :: args
			while (currentToken.kind == COMMA) {
				eat(COMMA)
				args = parseExpression :: args
			}
		}
		args
	}

    def parseNext(lhs: ExprTree): ExprTree = {
        currentToken.kind match {
            case DOT =>
                eat(DOT)
                currentToken.kind match {
                    case LENGTH =>
                        eat(LENGTH)
                        parseNext(new ArrayLength(lhs))
					case OR =>
						eat(OR)
						eat(LPAREN)
						val args = parseArgumentsMethod
						eat(RPAREN)
						lhs.getType match {
							case TObject(cs) =>
								cs.lookupMethod("||") match {
									case Some(ms) =>
										parseNext(new MethodCall(lhs, new Identifier("||"), args))
									case None =>
										fatal("|| method is not defined for Object " + lhs.toString)
								}
							case _ =>
								fatal("not an Object")
						}
					case AND =>
						eat(AND)
						eat(LPAREN)
						val args = parseArgumentsMethod
						eat(RPAREN)
						lhs.getType match {
							case TObject(cs) =>
								cs.lookupMethod("&&") match {
									case Some(ms) =>
										parseNext(new MethodCall(lhs, new Identifier("&&"), args))
									case None =>
										fatal("&& method is not defined for Object " + lhs.toString)
								}
							case _ =>
								fatal("not an Object")
						}
					case LESSTHAN =>
						eat(LESSTHAN)
						eat(LPAREN)
						val args = parseArgumentsMethod
						eat(RPAREN)
						lhs.getType match {
							case TObject(cs) =>
								cs.lookupMethod("<") match {
									case Some(ms) =>
										parseNext(new MethodCall(lhs, new Identifier("<"), args))
									case None =>
										fatal("< method is not defined for Object " + lhs.toString)
								}
							case _ =>
								fatal("not an Object")
						}
					case EQUALS =>
						eat(EQUALS)
						eat(LPAREN)
						val args = parseArgumentsMethod
						eat(RPAREN)
						lhs.getType match {
							case TObject(cs) =>
								cs.lookupMethod("==") match {
									case Some(ms) =>
										parseNext(new MethodCall(lhs, new Identifier("=="), args))
									case None =>
										fatal("== method is not defined for Object " + lhs.toString)
								}
							case _ =>
								fatal("not an Object")
						}
					case PLUS =>
						eat(PLUS)
						eat(LPAREN)
						val args = parseArgumentsMethod
						eat(RPAREN)
						lhs.getType match {
							case TObject(cs) =>
								cs.lookupMethod("+") match {
									case Some(ms) =>
										parseNext(new MethodCall(lhs, new Identifier("+"), args))
									case None =>
										fatal("+ method is not defined for Object " + lhs.toString)
								}
							case _ =>
								fatal("not an Object")
						}
					case MINUS =>
						eat(MINUS)
						eat(LPAREN)
						val args = parseArgumentsMethod
						eat(RPAREN)
						lhs.getType match {
							case TObject(cs) =>
								cs.lookupMethod("-") match {
									case Some(ms) =>
										parseNext(new MethodCall(lhs, new Identifier("-"), args))
									case None =>
										fatal("- method is not defined for Object " + lhs.toString)
								}
							case _ =>
								fatal("not an Object")
						}
					case TIMES =>
						eat(TIMES)
						eat(LPAREN)
						val args = parseArgumentsMethod
						eat(RPAREN)
						lhs.getType match {
							case TObject(cs) =>
								cs.lookupMethod("*") match {
									case Some(ms) =>
										parseNext(new MethodCall(lhs, new Identifier("*"), args))
									case None =>
										fatal("* method is not defined for Object " + lhs.toString)
								}
							case _ =>
								fatal("not an Object")
						}
					case DIV =>
						eat(DIV)
						eat(LPAREN)
						val args = parseArgumentsMethod
						eat(RPAREN)
						lhs.getType match {
							case TObject(cs) =>
								cs.lookupMethod("/") match {
									case Some(ms) =>
										parseNext(new MethodCall(lhs, new Identifier("/"), args))
									case None =>
										fatal("/ method is not defined for Object " + lhs.toString)
								}
							case _ =>
								fatal("not an Object")
						}
					case BANG =>
						eat(BANG)
						eat(LPAREN)
						val args = parseArgumentsMethod
						eat(RPAREN)
						lhs.getType match {
							case TObject(cs) =>
								cs.lookupMethod("!") match {
									case Some(ms) =>
										parseNext(new MethodCall(lhs, new Identifier("!"), args))
									case None =>
										fatal("! method is not defined for Object " + lhs.toString)
								}
							case _ =>
								fatal("not an Object")
						}
                    case _ =>
                        currentToken match {
                            case x: ID =>
                                eat(IDKIND)
                                eat(LPAREN)
                                val args = parseArgumentsMethod
                                eat(RPAREN)
                                parseNext(new MethodCall(lhs, new Identifier(x.value), args.reverse))
                            case _ =>
								expected(IDKIND)
                        }
                }
            case LBRACKET =>
                eat(LBRACKET)
                val index = parseExpression
                eat(RBRACKET)
                parseNext(new ArrayRead(lhs, index))
            case _ => lhs
        }
    }

    def parseNextExpr: ExprTree = parseNext(parseSimpleExpr)

    def parseSimpleExpr: ExprTree = {
        currentToken.kind match {
            case THIS =>
                eat(THIS)
                new This()
            case TRUE =>
                eat(TRUE)
                new True()
            case FALSE =>
                eat(FALSE)
                new False()
            case NEW =>
                eat(NEW)
                currentToken match {
                    case x: ID =>
                        val tpe = parseIdentifier
                        eat(LPAREN)
                        eat(RPAREN)
                        new New(tpe)
                    case _ =>
                        currentToken.kind match {
                            case INT =>
                                eat(INT)
                                eat(LBRACKET)
                                val expr = parseExpression
                                eat(RBRACKET)
                                new NewIntArray(expr)
                            case _ => expected(IDKIND, INT)
                        }
                }
            case LPAREN =>
                eat(LPAREN)
                val expr = parseExpression
                eat(RPAREN)
                expr
            case _ =>
                currentToken match {
                    case x: INTLIT =>
                        eat(INTLITKIND)
                        new IntLit(x.value)
                    case x: STRLIT =>
                        eat(STRLITKIND)
                        new StringLit(x.value)
                    case x: ID =>
                        eat(IDKIND)
                        new Identifier(x.value)
                    case _ => expected(INTLITKIND, STRLITKIND, IDKIND)
                }
        }
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
