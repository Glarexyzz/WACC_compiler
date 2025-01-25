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
        lexeme.symbol("int").map(_ => BaseType.IntType),
        lexeme.symbol("bool").map(_ => BaseType.BoolType),
        lexeme.symbol("char").map(_ => BaseType.CharType),
        lexeme.symbol("string").map(_ => BaseType.StringType)
    )

    lazy val pairElemType: Parsley[Type] =
    choice(
        baseType,                               
        arrayType,                                                        
        lexeme.symbol("pair").map(_ => ErasedPairType) 
    )

    lazy val pairType: Parsley[PairType] =
    lexeme.symbol("pair") *> (
        ('(' *> pairElemType <* lexeme.symbol(",")).flatMap { fst =>
            pairElemType.map { snd =>
                PairType(fst, snd)
            }
        } <* ')'
    )

    lazy val arrayType: Parsley[ArrayType] =
    (types <* lexeme.symbol("[") <* lexeme.symbol("]"))
        .map(inner => ArrayType(inner))

    lazy val types: Parsley[Type] =
    choice(
        baseType,
        arrayType,
        pairType
    )

    lazy val param: Parsley[Param] =
    (types <~> ident).map { case (paramType, name) =>
        Param(paramType, name)  //doesn't preserve result of ident??
    }
    // some syntax errors to resolve 
    /*
    lazy val skipStmt: Parsley[Statement] =
    lexeme.symbol("skip").map(_ => Skip)

    lazy val varDeclareStmt: Parsley[Statement] =
    (types <*> ident <* lexeme.symbol("=")).map { case (paramType, name) =>
        expr.map { rvalue =>
            VarDecl(paramType, name, RValue(rvalue))
        }
    }

    lazy val assignStmt: Parsley[Statement] = 
        (lvalue <* lexeme.symbol *> rvalue).map { case(lvalue, rvalue) => Assign(lvalue, rvalue)}


    lazy val readStmt: Parsley[Statement] =
    (lexeme.symbol("read") *> ident).map(ident => Read(LIdent(ident)))

    lazy val freeStmt: Parsley[Statement] = 
    (lexeme.symbol("free") *> expr).map(expr => Free(expr))

    lazy val returnStmt: Parsley[Statement] = 
    (lexeme.symbol("return") *> expr).map(expr => Return(expr))

    lazy val exitStmt: Parsley[Statement] = 
    (lexeme.symbol("exit") *> expr).map(expr => Exit(expr))

    lazy val printStmt: Parsley[Statement] =
    (lexeme.symbol("print") *> expr).map(expr => Print(expr, false))

    lazy val printlnStmt: Parsley[Statement] =
        (lexeme.symbol("println") *> expr).map(expr => Print(expr, true))

    lazy val ifStmt: Parsley[Statement] =
        (lexeme.symbol("if") *> expr <* lexeme.symbol("then") *> 
        stmt <* lexeme.symbol("else") *> 
        stmt <* lexeme.symbol("fi")).map {
        case (cond, thenStmt, elseStmt) => If(cond, thenStmt, elseStmt)
        }

    lazy val whileStmt: Parsley[Statement] =
        (lexeme.symbol("while") *> expr <* lexeme.symbol("do") *> 
        stmt <* lexeme.symbol("done")).map { case (cond, body) =>
        While(cond, body)
        }

    lazy val semicolonStmt: Parsley[Statement] =
    (stmt <* lexeme.symbol(";") *> stmt).map { case (firstStmt, secondStmt) =>
        Seq(firstStmt, secondStmt)
    }

    lazy val beginEndStmt: Parsley[Statement] =
    (lexeme.symbol("begin") *> stmt <* lexeme.symbol("end")).map(stmt => Block(stmt))

    lazy val stmt: Parsley[Statement]  = 
        skipStmt <|>
        varDeclareStmt <|>
        assignStmt
        readStmt <|>
        freeStmt <|>
        returnStmt <|>
        exitStmt <|>
        printStmt <|>
        printlnStmt <|>
        ifStmt <|>
        whileStmt <|>
        semicolonStmt <|>
        beginEndStmt
    */

        
}
