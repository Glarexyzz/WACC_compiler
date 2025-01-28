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

    val symbol = lexeme.symbol  //  shorten for hardKeyword & hardOps
    val softOp = lexeme.symbol.softOperator  //  shorten for softOps

    // Expression parsers

    // Unary Operators (Highest Precedence)
    val unaryOps: Parsley[UnaryOperator] =
        choice(
            symbol("!").as(UnaryOperator.Not),
            symbol("-").as(UnaryOperator.Negate),
            symbol("len").as(UnaryOperator.Length),
            symbol("ord").as(UnaryOperator.Ord),
            symbol("chr").as(UnaryOperator.Chr)
        )

    // Binary operators
    val mulOps: Parsley[BinaryOperator] =
        choice(
            symbol("*").as(BinaryOperator.Multiply),
            symbol("/").as(BinaryOperator.Divide),
            symbol("%").as(BinaryOperator.Modulus)
        )

    val addOps: Parsley[BinaryOperator] =
        choice(
            symbol("+").as(BinaryOperator.Add),
            symbol("-").as(BinaryOperator.Subtract)
        )

    val relOps: Parsley[BinaryOperator] =
        choice(
            symbol(">").as(BinaryOperator.Greater),
            symbol(">=").as(BinaryOperator.GreaterEqual),
            symbol("<").as(BinaryOperator.Less),
            symbol("<=").as(BinaryOperator.LessEqual)
        )

    val eqOps: Parsley[BinaryOperator] =
        choice(
            symbol("==").as(BinaryOperator.Equal),
            symbol("!=").as(BinaryOperator.NotEqual)
        )

    val andOps: Parsley[BinaryOperator] =
        symbol("&&").as(BinaryOperator.And)

    val orOps: Parsley[BinaryOperator] =
        symbol("||").as(BinaryOperator.Or)

    // Atom definition
    private lazy val atom: Parsley[Expr] =
        IntLiteral(intLiter) <|>
        BoolLiteral(boolLiter) <|>
        CharLiteral(charLiter) <|>
        StrLiteral(strLiter) <|>
        pairLiter.as(PairLiteral) <|>
        Identifier(ident) <|>
        arrayElem <|>
        '(' *> expr <* ')'

    // Array element definition
    private lazy val arrayElem: Parsley[ArrayElem] =
        ArrayElem(ident, some('[' *> expr <* ']'))

    // Expression definition
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
    
    // Types

    // Type definition
    private lazy val typeParser: Parsley[Type] =
        baseType <|> arrayType <|> pairType

    // Base type definition
    private lazy val baseType: Parsley[BaseType] = 
        choice(
            symbol("int").as(BaseType.IntType),
            symbol("bool").as(BaseType.BoolType),
            symbol("char").as(BaseType.CharType),
            symbol("string").as(BaseType.StrType)
        )

    // Array type definition
    private lazy val arrayType: Parsley[ArrayType] =
        (typeParser <* '[' <* ']').map(ArrayType)

    // Pair definition
    private lazy val pairType: Parsley[PairType] = 
        PairType(pairKeyword *> 
            '(' *> 
            pairElemType, (',' *> pairElemType) <* 
            ')'
        )

    // Pair element type definition
    private lazy val pairElemType: Parsley[PairElemType] = 
        baseTElem <|> arrayTElem <|> pairKeyword

    private lazy val baseTElem: Parsley[BaseTElem] =
        baseType.map(BaseTElem)

    private lazy val arrayTElem: Parsley[ArrayTElem] =
        arrayType.map(ArrayTElem)

    private lazy val pairKeyword: Parsley[PairElemType] = 
        lexeme.symbol("pair").as(PairKeyword)
}
