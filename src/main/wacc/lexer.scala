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
<<<<<<< Updated upstream
    val intLiter: Parsley[BigInt] = lexeme.signed.decimal32
=======
    val intLiter: Parsley[BigInt] = lexeme.signed.decimal // intSign with digits
>>>>>>> Stashed changes
    
    // Boolean
    val boolLiter: Parsley[Boolean] =                     // 'true' or 'false'
        lexeme.symbol("true").map(_ => true) <|>
        lexeme.symbol("false").map(_ => false)

    // Char & String
<<<<<<< Updated upstream
    val escapedChar: Parsley[Char] =
        char('0').as('\u0000') <|>
        char('b').as('\b') <|>
        char('t').as('\t') <|>
        char('n').as('\n') <|>
        char('f').as('\f') <|>
        char('r').as('\r') <|>
=======
    val escapedChar: Parsley[Char] =              // '0', 'b', 't', 'n', 'f', 'r', '\'', '"', '\\'
        char('0') <|>
        char('b') <|>
        char('t') <|>
        char('n') <|>
        char('f') <|>
        char('r') <|>
>>>>>>> Stashed changes
        char('\'') <|>
        char('"') <|>
        char('\\')

<<<<<<< Updated upstream
    val character: Parsley[Char] = 
        char('\\') *> escapedChar <|>
        satisfy(c => c != '\\' && c != '\'' && c != '"') 
        
    
    val charLiter: Parsley[Char] = //lexeme.character.ascii
        char('\'') *> character <* char('\'') <* 
        lexer.space.whiteSpace

    val strLiter: Parsley[String] = //lexeme.string.ascii
        (char('"') *> many(character) <* char('"'))
        .map(_.mkString) <* lexer.space.whiteSpace
=======
    val character: Parsley[Char] =                // any ACSII character except '\', ''' and '"' or '\'escapedChar
        satisfy(c => c != '\\' && c != '\'' && c != '"') <|>
        char('\\') *> escapedChar
    
    val charLiter: Parsley[Char] = lexeme.character.ascii // ''' character '''

    val strLiter: Parsley[String] =lexeme.string.ascii    // '"' character '"'
>>>>>>> Stashed changes

    // Null
    val pairLiter: Parsley[Unit] = lexeme.symbol("null")  // 'null'

    val ident: Parsley[String] = lexeme.names.identifier  //(letter | '_') (letter | digit | '_')*


    // Comments
    val eol: Parsley[Char] = endOfLine <|> crlf
    val eof: Parsley[Unit] = notFollowedBy(item)
    val comment: Parsley[Unit] = lexer.space.skipComments // '#'(any character except eol)* (<eol> | <eof>)

    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
