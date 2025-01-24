package wacc

import parsley.quick.*
import parsley.{Parsley, Result}
import parsley.expr.{precedence, Ops, InfixL, InfixN, InfixR, Prefix}
import parsley.syntax.character.charLift
import parsley.syntax.zipped.*

import lexer.{fully, intLiter, boolLiter, charLiter, strLiter, pairLiter, ident}
import lexer.lexeme


object parser {
    def parse(input: String): Result[String, BigInt] = parser.parse(input)
    //private val parser = fully(expr)

    /*   
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
            lexeme.symbol("!").as(UnaryOperator.Not),
            lexeme.symbol("-").as(UnaryOperator.Negate),
            lexeme.symbol("len").as(UnaryOperator.Length),
            lexeme.symbol("ord").as(UnaryOperator.Ord),
            lexeme.symbol("chr").as(UnaryOperator.Chr)
        )

    //  Binary Operators
    val mulOps: Parsley[BinaryOperator] =
        choice(
            lexeme.symbol("*").as(BinaryOperator.Multiply),
            lexeme.symbol("/").as(BinaryOperator.Divide),
            lexeme.symbol("%").as(BinaryOperator.Modulus)
        )

    val addOps: Parsley[BinaryOperator] =
        choice(
            lexeme.symbol("+").as(BinaryOperator.Add),
            lexeme.symbol("-").as(BinaryOperator.Subtract)
        )

    val relOps: Parsley[BinaryOperator] =
        choice(
            lexeme.symbol(">").as(BinaryOperator.Greater),
            lexeme.symbol(">=").as(BinaryOperator.GreaterEqual),
            lexeme.symbol("<").as(BinaryOperator.Less),
            lexeme.symbol("<=").as(BinaryOperator.LessEqual)
        )

    val eqOps: Parsley[BinaryOperator] =
        choice(
            lexeme.symbol("==").as(BinaryOperator.Equal),
            lexeme.symbol("!=").as(BinaryOperator.NotEqual)
        )

    val andOps: Parsley[BinaryOperator] =
        lexeme.symbol("&&").as(BinaryOperator.And)

    val orOps: Parsley[BinaryOperator] =
        lexeme.symbol("||").as(BinaryOperator.Or)

    //  <atom>
    private lazy val atom: Parsley[Expr] =
        IntLiteral(intLiter) <|>
        BoolLiteral(boolLiter) <|>
        CharLiteral(charLiter) <|>
        StrLiteral(strLiter) <|>
        pairLiter.map(_ => PairLiteral) <|>
        Identifier(ident) <|>
        arrayElem <|>
        '(' *> expr <* ')'

    //  <arrayElem>
    private lazy val arrayElem: Parsley[ArrayElem] =
        ArrayElem(ident, some('[' *> expr <* ']'))

    //  <expr>
    private lazy val expr: Parsley[Expr] = 
        precedence(atom)(
            Ops(Prefix)(unaryOps.map(op => (expr: Expr) => UnaryOp(op, expr))),
            Ops(InfixL)(mulOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right))),
            Ops(InfixL)(addOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right))),
            Ops(InfixN)(relOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right))),
            Ops(InfixN)(eqOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right))),
            Ops(InfixR)(andOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right))),
            Ops(InfixR)(orOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right)))
        )
    
}
