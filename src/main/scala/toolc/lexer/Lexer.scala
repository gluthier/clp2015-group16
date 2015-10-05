package toolc
package lexer

import utils._
import scala.io.Source
import java.io.File
import scala.io.BufferedSource

object Lexer extends Pipeline[File, Iterator[Token]] {

  import Tokens._

  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f)
    import ctx.reporter._

    def currentPos(): Positioned = {
      new Positioned {}.setPos(f, source.pos)
    }

    var droppedChar : Option[Char] = None

    def getChar(source: BufferedSource) = droppedChar match {
      case Some(x) =>
        droppedChar = None
        x
      case None => source.next()
    }

    def readNextToken(): Token = {
      var token: Token = null
      var p: Int = 0
      if (source hasNext) {
        val builder: StringBuilder = new StringBuilder
        var c: Char = getChar(source)
        p = source.pos

        if (c.isLetter) {
          builder append c
          if (source.hasNext) {
            c = getChar(source)
            while (source.hasNext && c.isLetterOrDigit || c == '_') {
              builder append c
              c = getChar(source)
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
        }
        else if (c.isDigit) {
          var k: Int = 0
          while (source.hasNext && c.isDigit) {
            k = (10 * k) + c.asDigit
            c = getChar(source)
          }
          if (!c.isDigit) {
            droppedChar = Some(c)
          }


          token = new INTLIT(k)
        }
        else if (c.isWhitespace) {
          return readNextToken()
        }
        else c match {
          case '=' =>
            val temp = getChar(source)
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
            val temp = getChar(source)
            if (temp == '&') {
              token = new Token(AND)
            }
            else {
              droppedChar = Some(temp)
              token = new Token(BAD)
              error("blbla", currentPos())
            }
          case '|' =>
            val temp = getChar(source)
            if (temp == '|') {
              token = new Token(OR)
            }
            else {
              droppedChar = Some(temp)
              token = new Token(BAD)
              error("blbla", currentPos())
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
            var temp = getChar(source)
            if (temp == '/') {
              temp = getChar(source)
              while (source.hasNext && temp != '\n') {
                temp = getChar(source)
              }
              return readNextToken()
            }
            else if (temp == '*') {

              temp = getChar(source)
              var temp2 = getChar(source)

              while (!(temp == '*' && temp2 == '/')) {
                temp = temp2
                temp2 = getChar(source)
              }
              return readNextToken()
            }
            else {
              droppedChar = Some(temp)
              token = new Token(DIV)
            }
          case '"' =>
            val b: StringBuilder = new StringBuilder
            var temp = getChar(source)
            while (source.hasNext && !(temp == '"') && temp != '\n') {
              b.append(temp)
              temp = getChar(source)
            }
            if (temp == '\n') {
              token = new Token(BAD)
              error("blbla", currentPos())
            }
            else {
              token = new STRLIT(b.mkString)
            }
          case _ =>
            token = new Token(BAD)
            error("Bad Token", currentPos())
        }


      }
      else {
        p = source.pos
        token = new Token(EOF)
      }
      token.setPos(currentPos().file, p)
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
