package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions.*
import parsley.character.{
    char, crlf, digit, endOfLine, item, letter, satisfy, string
}
import parsley.combinator.{option}

object lexer {
    private val desc = LexicalDesc.plain.copy(
        // your configuration goes here
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = Set("if")
        )
    )
    private val lexer = Lexer(desc)

    // Numbers
    val digit: Parsley[Char] = digit  // single digit '0'-'9'
    val intSign: Parsley[Char] = char('+') <|> char('-') // '+' or '-'
    val intLiter: Parsley[BigInt] = 
        (option(intSign) <~> some(digit)).map {
            case (some(intSign), digits) => 
                BigInt((sign.toString + digits.toString).toInt)
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
        satisfy(c => c != '\\' && c != '\'' && c != '"') <|>
        char('\\') *> escapedChar
    
    val charLiter: Parsley[Char] = 
        char('\'') *> character <* char('\'')

    val strLiter: Parsley[String] =
        (char('"') *> many(character) <* char('"')).map(_.toString)
    
    // Null
    val pairLiter: Parsley[String] =
        string("null")

    val ident: Parsley[String] = 
        {
            (char('_') <|> letter) *> 
            many((char('_') <|> letter <|> digit))
        }.map(_.toString)

    val eol: Parsley[Char] = endOfLine <|> crlf
    val eof: Parsley[Unit] = notFollowedBy(item)
    val comment: Parsley[String] = 
        {
            char('#') *> 
            many((satisfy(c => c != '\n' && c != '\r'))) <* 
            (eol <|> eof)
        }.map(_.toString)

    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
