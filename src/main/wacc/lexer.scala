package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions.*
import parsley.character.{anyChar, char, digit, letter, string}
import parsley.combinator.{eof, manyN, option}
import scala.math.Numeric.Implicits.infixNumericOps

object lexer {
    private val desc = LexicalDesc.plain.copy(
        // your configuration goes here
    )
    private val lexer = Lexer(desc)

    // Numbers
    val digit: Parsley[Char] = digit  // single digit '0'-'9'
    val intSign: Parsley[Char] = char('+') <|> char('-') // '+' or '-'
    val intLiter: Parsley[BigInt] = 
        (option(intSign) *> manyN(1, digit)).map {
            case (Some(sign), digits) => 
                BigInt((sign + digits.toString).toInt)
            case (None, digits)       => 
                BigInt(digits.toString.toInt)
        }
    
    // Boolean
    val boolLiter: Parsley[Boolean] =
        (char('t') *> char('r') *> char('u') *> char('e')).map(_ => true) <|>
        (char('f') *> char('a') *> char('l') *> char('s') *> char('e')).map(_ => false)

    // Char & String
    val escapedChar: Parsley[Char] =
        char('0') <|>
        char('b') <|>
        char('t') <|>
        char('n') <|>
        char('f') <|>
        char('r') <|>
        char('\'') <|>
        char('"') <|>
        char('\\')

    val character: Parsley[Char] = 
        (anyChar - (char('\\') <|> char('\'') <|> char('"'))) <|>
        char('\\') ~ escapedChar
    
    val charLiter: Parsley[Char] = 
        char('\'') *> character <* char('\'')

    val strLiter: Parsley[String] =
        (char('"') *> manyN(0, character) <* char('"')).map(_.toString)
    
    // Null
    val pairLiter: Parsley[String] =
        string("null")

    val ident: Parsley[String] = 
        {
            (char('_') <|> letter) *> 
            manyN(0, (char('_') <|> letter <|> digit))
        }.map(_.toString)

    val eol: Parsley[Char] = 
        char('\n') <|> (char('\r') *> option(char('\n')))
    val comment: Parsley[String] = 
        (char('#') *> manyN(0, (anyChar - eol)) <* (eol <|> eof)).map(_.toString)

    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
