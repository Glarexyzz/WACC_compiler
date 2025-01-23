package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions.*
import parsley.character.{char, digit}
import parsley.combinator.{many1, option}

object lexer {
    private val desc = LexicalDesc.plain.copy(
        // your configuration goes here
    )
    private val lexer = Lexer(desc)

    val digit: Parsley[Char] = digit  // single digit '0'-'9'
    val intSign: Parsley[Char] = char('+') <|> char('-') // '+' or '-'
    val intLiter: Parsley[BigInt] = 
        (option(intSign) ~ many1(digit)).map {
            case (Some(sign), digits) => 
                BigInt((sign + digits.mkString).toInt)
            case (None, digits)       => 
                BigInt(digits.mkString.toInt)
        }

    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
