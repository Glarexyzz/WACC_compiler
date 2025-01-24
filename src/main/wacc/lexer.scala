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
            hardKeywords = Set(
                "if", "begin", "end", "is", "skip", "read", "free", 
                "return", "exit", "print", "println", "then", "else",
                "fi", "while", "do", "done", "newpair", "call", "fst",
                "snd", "int", "bool", "char", "string", "pair", "null",
                "true", "false", "len", "ord", "chr"
            ),
            hardOperators = Set(
                "!", "-", "len", "ord", "chr", "*", "/", "%", "+",
                ">", ">=", "<", "<=", "==", "!=", "&&", "||"
            )
        ),
        textDesc = TextDesc.plain.copy(
            characterLiteralEnd = '\'',
            stringEnds = Set(("\"", "\""))
        ),
        spaceDesc = SpaceDesc.plain.copy(
            lineCommentAllowsEOF = true,
            lineCommentStart = "#",
            space = Basic(_.isWhitespace)
        )
    )
    private val lexer = Lexer(desc)

    // Numbers
    val digit: Parsley[Char] = digit  // single digit '0'-'9'
    val intSign: Parsley[Char] = char('+') <|> char('-') // '+' or '-'
    val intLiter: Parsley[BigInt] = lexer.lexeme.signed.decimal
    
    // Boolean
    val boolLiter: Parsley[Boolean] =
        lexer.lexeme.symbol("true").map(_ => true) <|>
        lexer.lexeme.symbol("false").map(_ => false)

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
    
    val charLiter: Parsley[Char] = lexer.lexeme.character.ascii

    val strLiter: Parsley[String] =lexer.lexeme.string.ascii

    // Null
    val pairLiter: Parsley[Unit] = lexer.lexeme.symbol("null")

    val ident: Parsley[String] = lexer.lexeme.names.identifier

    // Comments
    val eol: Parsley[Char] = endOfLine <|> crlf
    val eof: Parsley[Unit] = notFollowedBy(item)
    val comment: Parsley[Unit] = lexer.space.skipComments

    val types: Parsley[String] = baseType <|> arrayType <|> pairType

    val arrayType: Parsley[String] = types <* char('[') <* char(']')

    val pairElemType: Parsley[String] = baseType <|> arrayType <|> lexer.lexeme.symbol.softKeyword("pair").map(_ => "pair")
    
    val pairType: Parsley[String] = 
    lexer.lexeme.symbol.softKeyword("pair") *>
    char('(') *>
    pairElemType <* char(',') <* pairElemType <* char(')')
    
    val baseType: Parsley[String] = 
    lexer.lexeme.symbol.softKeyword("int").map(_ => "int") <|>
    lexer.lexeme.symbol.softKeyword("bool").map(_ => "bool") <|>
    lexer.lexeme.symbol.softKeyword("char").map(_ => "char") <|>
    lexer.lexeme.symbol.softKeyword("string").map(_ => "string")


    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
