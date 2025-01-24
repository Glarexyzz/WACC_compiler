package wacc

import parsley.quick.some
import parsley.{Parsley, Result}
import parsley.expr.{precedence, Ops, InfixL, InfixN, InfixR, Prefix}
import parsley.syntax.character.charLift
import parsley.syntax.zipped.*
import parsley.combinator._
import lexer.{fully, intLiter, boolLiter, charLiter, strLiter, pairLiter, ident}
import lexer.lexeme


object parser {
    def parse(input: String): Result[String, BigInt] = parser.parse(input)
    //private val parser = fully(expr)

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
    
    lazy val baseType: Parsley[BaseType] =
    choice(
        lexer.lexeme.symbol("int").map(_ => BaseType.IntType),
        lexer.lexeme.symbol("bool").map(_ => BaseType.BoolType),
        lexer.lexeme.symbol("char").map(_ => BaseType.CharType),
        lexer.lexeme.symbol("string").map(_ => BaseType.StringType)
    )

    /*
    lazy val pairElemType: Parsley[Type] =
        baseType <|>
        arrayType <|>
        lexer.lexeme.symbol("pair").map(_ => PairType)
    
    lazy val pairType: Parsley[PairType] =
    (lexer.lexeme.symbol("pair") *>
        ('(' *> pairElemType <* lexer.lexeme.symbol(",") <~ pairElemType <* ')'))
        .map { case (fst, snd) => PairType(fst, snd) }

    */
    lazy val arrayType: Parsley[ArrayType] =
        (types <* lexer.lexeme.symbol("[") <* lexer.lexeme.symbol("]"))
            .map(e => ArrayType(e))

  
    lazy val types: Parsley[Type] =
        choice(
            baseType,
            arrayType,
            //pairType
        )

        
}
