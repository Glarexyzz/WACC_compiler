package wacc

import parsley.quick.*
import parsley.Parsley
import parsley.token.{Lexer, Basic}
import parsley.token.descriptions.*
import parsley.character.{
    char, crlf, endOfLine, item, satisfy
}

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
                "true", "false", ",", "(", ")", "[", "]"
            ),
            hardOperators = Set(
                "!", "-", "len", "ord", "chr", "*", "/", "%", "+",
                ">", ">=", "<", "<=", "==", "!=", "&&", "||", ";"
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
    val lexer = Lexer(desc)

    val lexeme = lexer.lexeme // For parser to use lexeme

    // Numbers
    val digit: Parsley[Char] = digit  // single digit '0'-'9'
    val intSign: Parsley[Char] = char('+') <|> char('-') // '+' or '-'
    val intLiter: Parsley[BigInt] = lexeme.signed.decimal
    
    // Boolean
    val boolLiter: Parsley[Boolean] =
        lexeme.symbol("true").map(_ => true) <|>
        lexeme.symbol("false").map(_ => false)

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
    
    val charLiter: Parsley[Char] = lexeme.character.ascii

    val strLiter: Parsley[String] =lexeme.string.ascii

    // Null
    val pairLiter: Parsley[Unit] = lexeme.symbol("null")

    val ident: Parsley[String] = lexeme.names.identifier


    // Comments
    val eol: Parsley[Char] = endOfLine <|> crlf
    val eof: Parsley[Unit] = notFollowedBy(item)
    val comment: Parsley[Unit] = lexer.space.skipComments

    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
