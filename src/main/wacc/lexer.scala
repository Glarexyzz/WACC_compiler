package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions.*
import parsley.character.{any, char, digit, letter}
import parsley.combinator.{many, manyN, option}

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
                BigInt((sign + digits.mkString).toInt)
            case (None, digits)       => 
                BigInt(digits.mkString.toInt)
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
        (any - (char('\\') <|> char('\'') <|> char('"'))) <|>
        char('\\') ~ escapedChar
    
    val charLiter: Parsley[Char] = 
        char('\'') *> character <* char('\'')

    val strLiter: Parsley[String] =
        (char('"') *> many(character) <* char('"')).map(_.mkString)
    
    // Null
    val pairLiter: Parsley[Unit] =
        char('n') *> char('u') *> char('l') *> char('l')

    val ident: Parsley[String] = 
        {
            (char('_') <|> letter) ~ 
            many(char('_') <|> letter <|> digit)
        }.map(_.mkString)

    val eol: Parsley[Unit] = 
        char('\n') <|> (char('\r') *> option(char('\n')))
    val eof: Parsley[Unit] = endOfInput
    val comment: Parsley[String] = 
        (char('#') *> many(any - eol) <* (eol <|> eof)).map(_.mkString)

    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
