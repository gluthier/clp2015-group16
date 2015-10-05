package toolc
package lexer

import utils._
import scala.io.Source
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {
    import Tokens._

    def run(ctx: Context)(f: File): Iterator[Token] = {
        val source = Source.fromFile(f)
        import ctx.reporter._

        def currentPos(): Positioned = {
            new Positioned{}.setPos(f, source.pos)
        }

        def readNextToken(): Token = {
            val c : //read character
            c match {
                case ":" => new Token(COLON)
                case ";" => new Token(SEMICOLON)
                case "." => new Token(DOT)
                case "," => new Token(COMMA)
                case "=" => //check if next char also "="
                case "!" => new Token(BANG)
                case "(" => new Token(LPAREN)
                case ")" => new Token(RPAREN)
                case "[" => new Token(LBRACKET)
                case "]" => new Token(RBRACKET)
                case "{" => new Token(LBRACE)
                case "}" => new Token(RBRACE)
                case "&" => //check if next char also "&"
                case "|" => //check if next char also "|"
                case "<" => new Token(LESSTHAN)
                case "+" => new Token(PLUS)
                case "-" => new Token(MINUS)
                case "*" => new Token(TIMES)
                case "/" => new Token(DIV)
                case _ =>
            }
        }

        new Iterator[Token] {
            var nextToken: Token = readNextToken
            var reachedEnd = false

            def hasNext = {
                nextToken.kind != EOF || !reachedEnd
            }

            def next = {
                val r = nextToken
                nextToken = readNextToken
                if (r.kind == EOF) {
                    reachedEnd = true
                }
                r
            }
        }
    }
}
