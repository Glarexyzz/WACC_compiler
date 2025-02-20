package wacc

import parsley.quick.*
import parsley.Parsley
import parsley.token.{Lexer, Basic}
import parsley.token.descriptions.*
import parsley.character.{char, satisfy}
import parsley.errors.combinator.ErrorMethods

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
    val digit: Parsley[Char] = digit                        // single digit '0'-'9'
    val intSign: Parsley[Char] = (char('+') <|> char('-')).label("sign")    // '+' or '-'
    val intLiter: Parsley[BigInt] = lexeme.signed.decimal32 // intSign with digits
    
    // Boolean
    val boolLiter: Parsley[Boolean] =                     // 'true' or 'false'
        lexeme.symbol("true").map(_ => true).label("the boolean value 'true'") <|>
        lexeme.symbol("false").map(_ => false).label("the boolean value 'false'")

    // Char & String
    val escapedChar: Parsley[Char] =
        (char('0').as('\u0000') <|> // '0', 'b', 't', 'n', 'f', 'r', '\'', '"', '\\'
        char('b').as('\b') <|>
        char('t').as('\t') <|>
        char('n').as('\n') <|>
        char('f').as('\f') <|>
        char('r').as('\r') <|>
        char('\'') <|>
        char('"') <|>
        char('\\')).label("escaped character")

    val character: Parsley[Char] = // any ACSII character except '\', ''' and '"' or '\'escapedChar
        char('\\') *> escapedChar <|>
        satisfy(
            c => 
            c != '\\' && 
            c != '\'' &&
            c != '"' && 
            c != '\n' && 
            c != '\r' &&
            c >= 0x00.toChar &&
            c <= 0x7F.toChar
        ).label("a valid character enclosed in single quotes")
            .explain("Characters must be ASCII")
    
    val charLiter: Parsley[Char] = //lexeme.character.ascii
        (char('\'') *> character <* char('\'') <* 
        lexer.space.whiteSpace).label("char")

    val strLiter: Parsley[String] = //lexeme.string.ascii
        ((char('"') *> many(character) <* char('"'))
        .map(_.mkString) <* lexer.space.whiteSpace).label("string")

    // Null
    val pairLiter: Parsley[Unit] = lexeme.symbol("null")  // 'null'

    val ident: Parsley[String] = lexeme.names.identifier  //(letter | '_') (letter | digit | '_')*
        .label("valid identifier")
        .explain("must start with '_' or letter")

    // Comments
    val comment: Parsley[Unit] = lexer.space.skipComments // '#'(any character except eol)* (<eol> | <eof>)

    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
