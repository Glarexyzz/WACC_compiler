package wacc

import parsley.quick.*
import parsley.Parsley
import parsley.token.{Lexer, Basic}
import parsley.token.descriptions.*
import parsley.character.{
    char, crlf, endOfLine, item, satisfy
}
// for error checking
// import parsley.token.errors.* 

object lexer {
    // private val errorConfig = new ErrorConfig {
    //     override def labelSymbol = Map(
    //         "}" -> LabelAndReason(reason = "unclosed braces", label = "closing braces")
            
    //     )
    //     // errors: syntax and semantic
    //     // stuff like incomplete brackets, no begin and end, and so on
    // }

    private val desc = LexicalDesc.plain.copy(
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
                "true", "false"
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
    val digit: Parsley[Char] = digit                      // single digit '0'-'9'
    val intSign: Parsley[Char] = char('+') <|> char('-')  // '+' or '-'
    val maxIntValue: BigInt = BigInt(2147483647)
    val minIntValue: BigInt = BigInt(-2147483648)
    val intLiter: Parsley[BigInt] = 
        lexeme.signed.decimal.filter(
            n => n <= maxIntValue && n >= minIntValue
        )
    
    // Boolean
    val boolLiter: Parsley[Boolean] =
        lexeme.symbol("true").map(_ => true) <|>
        lexeme.symbol("false").map(_ => false)

    // Char & String
    val escapedChar: Parsley[Char] =
        char('0').as('\u0000') <|>
        char('b').as('\b') <|>
        char('t').as('\t') <|>
        char('n').as('\n') <|>
        char('f').as('\f') <|>
        char('r').as('\r') <|>
        char('\'') <|>
        char('"') <|>
        char('\\')

    val character: Parsley[Char] = 
        char('\\') *> escapedChar <|>
        satisfy(c => c != '\\' && c != '\'' && c != '"') 
        
    
    val charLiter: Parsley[Char] = //lexeme.character.ascii
        char('\'') *> character <* char('\'') <* 
        lexer.space.whiteSpace

    val strLiter: Parsley[String] = //lexeme.string.ascii
        (char('"') *> many(character) <* char('"'))
        .map(_.mkString) <* lexer.space.whiteSpace

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
