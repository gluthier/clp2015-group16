package toolc
package lexer

import utils._
import scala.io.Source
import java.io.File
import scala.io.BufferedSource

object Lexer extends Pipeline[File, Iterator[Token]] {

  import Tokens._

  val map: Map[String, TokenKind] = Map(
        "object" -> Tokens.OBJECT,
        "class" -> Tokens.CLASS,
        "def" -> Tokens.DEF,
        "var" -> Tokens.VAR,
        "Unit" -> Tokens.UNIT,
        "main" -> Tokens.MAIN,
        "String" -> Tokens.STRING,
        "extends" -> Tokens.EXTENDS,
        "Int" -> Tokens.INT,
        "Bool" -> Tokens.BOOLEAN,
        "while" -> Tokens.WHILE,
        "if" -> Tokens.IF,
        "else" -> Tokens.ELSE,
        "return" -> Tokens.RETURN,
        "length" -> Tokens.LENGTH,
        "true" -> Tokens.TRUE,
        "false" -> Tokens.FALSE,
        "this" -> Tokens.THIS,
        "new" -> Tokens.NEW,
        "println" -> Tokens.PRINTLN
  )

  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f)
    import ctx.reporter._

    def currentPos(): Positioned = {
      new Positioned {}.setPos(f, source.pos)
    }

    var droppedChar : Option[Char] = None

    def readChar(source: BufferedSource) = droppedChar match {
      case Some(x) =>
        droppedChar = None
        x
      case None => source.next()
    }

    def readNextToken(): Token = {
      var token: Token = null
      var position: Int = 0

      if (source hasNext) {
        val builder: StringBuilder = new StringBuilder
        var c: Char = readChar(source)
        position = source.pos

        if (c.isLetter) {
          builder append c
          if (source.hasNext) {
            c = readChar(source)
            while (source.hasNext && c.isLetterOrDigit || c == '_') {
              builder append c
              c = readChar(source)
            }
            if (!c.isLetterOrDigit) {
              droppedChar = Some(c)
            }
          }
          val tokenString = builder.mkString
          map.get(tokenString) match {
            case Some(x: TokenKind) => token = new Token(x)
            case None => token = new ID(tokenString)
          }
        } else if (c.isDigit) {
          var k: Int = 0
          while (source.hasNext && c.isDigit) {
            k = (10 * k) + c.asDigit
            c = readChar(source)
          }
          if (!c.isDigit) {
            droppedChar = Some(c)
          }

          token = new INTLIT(k)
        } else if (c.isWhitespace) {
          return readNextToken()
        } else c match {
          case '=' =>
            val temp = readChar(source)
            if (temp == '=') {
              token = new Token(EQUALS)
            } else {
              droppedChar = Some(temp)
              token = new Token(EQSIGN)
            }
          case ':' =>
            token = new Token(COLON)
          case ';' =>
            token = new Token(SEMICOLON)
          case '.' =>
            token = new Token(DOT)
          case ',' =>
            token = new Token(COMMA)
          case '!' =>
            token = new Token(BANG)
          case '(' =>
            token = new Token(LPAREN)
          case ')' =>
            token = new Token(RPAREN)
          case '[' =>
            token = new Token(LBRACKET)
          case ']' =>
            token = new Token(RBRACKET)
          case '{' =>
            token = new Token(LBRACE)
          case '}' =>
            token = new Token(RBRACE)
          case '&' =>
            val temp = readChar(source)
            if (temp == '&') {
              token = new Token(AND)
            } else {
              droppedChar = Some(temp)
              token = new Token(BAD)
              error("Bad Token", currentPos)
            }
          case '|' =>
            val temp = readChar(source)
            if (temp == '|') {
              token = new Token(OR)
            } else {
              droppedChar = Some(temp)
              token = new Token(BAD)
              error("Bad Token", currentPos)
            }
          case '<' =>
            token = new Token(LESSTHAN)
          case '+' =>
            token = new Token(PLUS)
          case '-' =>
            token = new Token(MINUS)
          case '*' =>
            token = new Token(TIMES)
          case '/' =>
            var temp = readChar(source)
            if (temp == '/') {
              temp = readChar(source)
              while (source.hasNext && temp != '\n') {
                temp = readChar(source)
              }
              return readNextToken()
            } else if (temp == '*') {

              temp = readChar(source)
              var temp2 = readChar(source)

              while (!(temp == '*' && temp2 == '/')) {
                temp = temp2
                temp2 = readChar(source)
              }
              return readNextToken()
            } else {
              droppedChar = Some(temp)
              token = new Token(DIV)
            }
          case '"' =>
            val builder: StringBuilder = new StringBuilder
            var temp = readChar(source)
            while (source.hasNext && !(temp == '"') && temp != '\n') {
              builder append temp
              temp = readChar(source)
            }
            if (temp == '\n') {
              token = new Token(BAD)
              error("Bad Token", currentPos)
            } else {
              token = new STRLIT(b.mkString)
            }
          case _ =>
            token = new Token(BAD)
            error("Bad Token", currentPos)
        }
      } else {
        position = source.pos
        token = new Token(EOF)
      }
      token.setPos(currentPos.file, position)
      token
    }

    new Iterator[Token] {
      var nextToken: Token = readNextToken()
      var reachedEnd = false

      def hasNext = {
        nextToken.kind != EOF || !reachedEnd
      }

      def next() = {
        val r = nextToken
        nextToken = readNextToken()
        if (r.kind == EOF) {
          reachedEnd = true
        }
        r
      }
    }
  }
}
