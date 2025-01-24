package wacc

import parsley.quick.*
import parsley.{Parsley, Result}
import parsley.expr.chain
import parsley.expr.{precedence, Ops, InfixL, InfixN, InfixR, Prefix}
import parsley.syntax.character.charLift

import lexer.implicits.implicitSymbol
import lexer.{digit, fully, intLiter, boolLiter, charLiter, strLiter, pairLiter, ident}


object parser {
    def parse(input: String): Result[String, BigInt] = parser.parse(input)
    //private val parser = fully(expr)
    
    //  Binary Operators
    private val add = (x: BigInt, y: BigInt) => x + y
    private val sub = (x: BigInt, y: BigInt) => x - y
    private val mul = (x: BigInt, y: BigInt) => x * y
    private val div = (x: BigInt, y: BigInt) => x / y
    private val mod = (x: BigInt, y: BigInt) => x % y
    private val and = (x: Boolean, y: Boolean) => x && y
    private val or = (x: Boolean, y: Boolean) => x || y
    private val eq = (x: Any, y: Any) => x == y
    private val neq = (x: Any, y: Any) => x != y
    private val gt = (x: BigInt, y: BigInt) => x > y
    private val gte = (x: BigInt, y: BigInt) => x >= y
    private val lt = (x: BigInt, y: BigInt) => x < y
    private val lte = (x: BigInt, y: BigInt) => x <= y

    // Unary Operators
    private val not = (x: Boolean) => !x
    private val negate = (x: BigInt) => -x
    /*
    private lazy val atom: Parsley[Expr] =
    intLiter.map(IntLiteral) |
    boolLiter.map(BoolLiteral) |
    charLiter.map(CharLiteral) |
    strLiter.map(StrLiteral) |
    pairLiter.map(_ => PairLiteral()) |
    ident.map(Identifier) |
    arrayElem.map { case (name, indices) => ArrayElem(name, indices) } |
    "(" ~> expr.map(ParenExpr) <~ ")"
    */
    /*
    private lazy val expr: Parsley[BigInt] =
        chain.left1(digit | "(" ~> expr <~ ")")(
            ("+" as add) | ("-" as sub)
        )
    */
    /*
    private lazy val expr: Parsley[Expr] = precedence[Expr](
        Ops(Prefix)(not, "!", "len", "ord", "chr"),        // Unary operators
        Ops(InfixL)(mul, "*", "/", "%"),                 // Multiplicative
        Ops(InfixL)(add, "+", "-"),                      // Additive
        Ops(InfixN)(gt, ">", gte, ">=", lt, "<", lte, "<="), // Relational
        Ops(InfixN)(eq, "==", neq, "!="),                // Equality
        Ops(InfixR)(and, "&&"),                          // Logical AND
        Ops(InfixR)(or, "||")                            // Logical OR
    )
    */


    //  Parser for expressions  

    //  Unary Operators (Highest Precedence)
    val unaryOps: Parsley[UnaryOperator] =
        choice(
            lexer.lexeme.symbol("!").map(UnaryOperator.Not),
            lexer.lexeme.symbol("-").map(UnaryOperator.Negate),
            lexer.lexeme.symbol("len").map(UnaryOperator.Length),
            lexer.lexeme.symbol("ord").map(UnaryOperator.Ord),
            lexer.lexeme.symbol("chr").map(UnaryOperator.Chr)
        )

    //  Binary Operators
    val mulOps: Parsley[BinaryOperator] =
        choice(
            lexer.lexeme.symbol("*").map(BinaryOperator.Multiply),
            lexer.lexeme.symbol("/").map(BinaryOperator.Divide),
            lexer.lexeme.symbol("%").map(BinaryOperator.Modulus)
        )

    val addOps: Parsley[BinaryOperator] =
        choice(
            lexer.lexeme.symbol("+").map(BinaryOperator.Add),
            lexer.lexeme.symbol("-").map(BinaryOperator.Subtract)
        )

    val relOps: Parsley[BinaryOperator] =
        choice(
            lexer.lexeme.symbol(">").map(BinaryOperator.Greater),
            lexer.lexeme.symbol(">=").map(BinaryOperator.GreaterEqual),
            lexer.lexeme.symbol("<").map(BinaryOperator.Less),
            lexer.lexeme.symbol("<=").map(BinaryOperator.LessEqual)
        )

    val eqOps: Parsley[BinaryOperator] =
        choice(
            lexer.lexeme.symbol("==").map(BinaryOperator.Equal),
            lexer.lexeme.symbol("!=").map(BinaryOperator.NotEqual)
        )

    val andOps: Parsley[BinaryOperator] =
        lexer.lexeme.symbol("&&").map(BinaryOperator.And)

    val orOps: Parsley[BinaryOperator] =
        lexer.lexeme.symbol("||").map(BinaryOperator.Or)

    //  <atom>
    private lazy val atom: Parsley[Expr] =
        intLiter.map(IntLiteral) <|>
        boolLiter.map(BoolLiteral) <|>
        charLiter.map(CharLiteral) <|>
        strLiter.map(StringLiteral) <|>
        pairLiter.map(_ => PairLiteral) <|>
        ident.map(Identifier) <|>
        arrayElem <|>
        '(' *> expr <* ')'

    // <arrayElem>
    private lazy val arrayElem: Parsley[ArrayElem] =
        (ident, some('[' *> expr <* ']')).map(ArrayElem)

}
