package wacc

import parsley.quick.*
import parsley.Parsley
import parsley.token.{Lexer, Basic}
import parsley.token.descriptions.*
import parsley.character.{
    char, crlf, digit, endOfLine, item, letter, satisfy, string
}
import parsley.combinator.{option}

object lexer {
    private val desc = LexicalDesc.plain.copy(
        // your configuration goes here
        nameDesc = NameDesc.plain.copy(
            identifierStart = Basic(c => c.isLetter || c == '_'),
            identifierLetter = Basic(c => c.isLetterOrDigit || c == '_'),
        ),
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = Set("if")
        )
    )
    private val lexer = Lexer(desc)

    // Numbers
    val digit: Parsley[Char] = digit  // single digit '0'-'9'
    val intSign: Parsley[Char] = char('+') <|> char('-') // '+' or '-'
    val intLiter: Parsley[BigInt] = lexer.lexeme.integer.signed
    
    // Boolean
    val boolLiter: Parsley[Boolean] =
        lexer.lexeme.names.reserved("true").map(_ => true) <|>
        lexer.lexeme.names.reserved("false").map(_ => false)

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
    
    val charLiter: Parsley[Char] = lexer.lexeme.charLiteral

    val strLiter: Parsley[String] =lexer.lexeme.stringLiteral

    // Null
    val pairLiter: Parsley[String] = lexer.lexeme.names.reserved("null")

    val ident: Parsley[String] = lexer.lexeme.names.identifier

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
