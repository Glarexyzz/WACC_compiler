package wacc

import parsley.{Parsley}
import parsley.expr.{precedence, Ops, InfixL, InfixN, InfixR, Prefix}
import parsley.quick.*

// for testing
import parsley.errors.ErrorBuilder

import lexer.{fully, intLiter, boolLiter, charLiter, strLiter, pairLiter, ident}

object parser {
    def parse[Err >: ParserError: ErrorBuilder](prog: String): Either[Err, Any] = {
        val parsers: List[(String, Parsley[Any])] = List(
            "Expression" -> fully(expr),
            "Statement" -> fully(stmt),
            "Function" -> fully(func),
            "Program" -> fully(program)
            
        )


        var lastError: Option[Err] = None
        parsers.foldLeft(Option.empty[Either[Err, Any]]) {
            case (Some(result), _) => Some(result) // Parsing succeeded!
            case (None, (name, parser)) => 
                parser.parse(prog) match {
                    case parsley.Success(result) => 
                        println(s"Success! Parsed as: $name")
                        Some(Right(result))
                    case parsley.Failure(err) => // there seems to be a conflict between the Err from parsley and the Err in Option[Err]
                        lastError = Some(err)
                        None
                        // None
                        // currently it will keep trying to parse, but next time, 
                        // we want it to fail the moment we find a syntax error, 
                        // but continue for a semantic error (and report multiple semantic errors)
                }
        }.getOrElse{
            Left(lastError.getOrElse{
                ParserError((1,1), "<input>", ParserErrorLines.SpecialisedError(Set("All parsers failed"), 0))
            })
            // 
        }

    }


    // Individual parsing functions
    def parseProgram(input: String): Either[String, Program] =
        fully(program).parse(input).toEither

    def parseFunc(input: String): Either[String, Func] =
        fully(func).parse(input).toEither

    def parseStmt(input: String): Either[String, Stmt] =
        fully(stmt).parse(input).toEither

    def parseExpr(input: String): Either[String, Expr] =
        fully(expr).parse(input).toEither

    private lazy val symbol = lexer.lexeme.symbol  // shorten for hardKeyword & hardOps
    val softOp = lexer.lexeme.symbol.softOperator  // shorten for softOps

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
        (symbol("(") *> expr <* symbol(")"))

    // Array element definition
    private lazy val arrayElem: Parsley[ArrayElem] =
        ArrayElem(ident, some(symbol("[") *> expr <* symbol("]")))

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
        baseType <|> pairType <|> arrayType 

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
        (baseType <|> pairType).flatMap { t =>
            some(symbol("[") *> symbol("]")).map(_ => ArrayType(t))
        }

    // Pair definition
    private lazy val pairType: Parsley[PairType] = 
        PairType(
            (symbol("pair") *> symbol("(") *> pairElemType), 
            (symbol(",") *> pairElemType <* symbol(")"))
        )

    // Pair element type definition
    private lazy val pairElemType: Parsley[PairElemType] = 
        pairKeyword <|> baseTElem <|> arrayTElem  

    private lazy val baseTElem: Parsley[BaseTElem] =
        BaseTElem(baseType)

    private lazy val arrayTElem: Parsley[ArrayTElem] =
        ArrayTElem(arrayType)

    private lazy val pairKeyword: Parsley[PairElemType] = 
        symbol("pair").as(PairKeyword)

    // Statements

    // Program definition

    private lazy val program: Parsley[Program] = 
        Program(
            (symbol("begin") *> many(func)), 
            (stmt <* symbol("end"))
        )

    // Func definition
    private lazy val func: Parsley[Func] = 
        Func(  
            typeParser, 
            ident, 
            (symbol("(") *> option(paramList) <* symbol(")")),
            (symbol("is") *> stmt <* symbol("end"))
        )
    
    // ParamList definition
    private lazy val paramList: Parsley[List[Param]] = sepBy1(param, symbol(","))
    

    // Param definition
    private lazy val param: Parsley[Param] = Param(typeParser, ident)

    // Statement definition
    private lazy val stmtAtom: Parsley[Stmt] =
        symbol("skip").as(SkipStmt) <|>
        DeclAssignStmt(typeParser, ident, (softOp("=") *> rValue)) <|>
        AssignStmt(lValue, (softOp("=") *> rValue)) <|>
        ReadStmt(symbol("read") *> lValue) <|>
        FreeStmt(symbol("free") *> expr) <|>
        ReturnStmt(symbol("return") *> expr) <|>
        ExitStmt(symbol("exit") *> expr) <|>
        PrintStmt(symbol("print") *> expr) <|>
        PrintlnStmt(symbol("println") *> expr) <|>
        IfStmt(
            (symbol("if") *> expr), 
            (symbol("then") *> stmt), 
            (symbol("else") *> stmt <* symbol("fi"))
        ) <|>
        WhileStmt(
            (symbol("while") *> expr),
            (symbol("do") *> stmt <* symbol("done"))
        ) <|>
        BodyStmt(symbol("begin") *> stmt <* symbol("end"))
    
    private lazy val stmt: Parsley[Stmt] = 
        precedence(stmtAtom)(
            Ops(InfixL)(SeqStmt from symbol(";"))
        )

    // Left value definition
    private lazy val lValue: Parsley[LValue] = 
        lName <|> lArray <|> lPair

    private lazy val lName: Parsley[LValue] = LValue.LName(ident)
    private lazy val lArray: Parsley[LValue] = LValue.LArray(arrayElem)
    private lazy val lPair: Parsley[LValue] = LValue.LPair(pairElem)

    // Right value definition
    private lazy val rValue: Parsley[RValue] =
        rExpr <|> 
        rArrayLiter <|>
        rNewPair <|>
        rPair <|>
        rCall

    private lazy val rExpr: Parsley[RValue] = RValue.RExpr(expr)
    private lazy val rArrayLiter: Parsley[RValue] = RValue.RArrayLiter(arrayLiter)
    private lazy val rNewPair: Parsley[RValue] = 
        RValue.RNewPair(
            (symbol("newpair") *> symbol("(") *> expr),
            (symbol(",") *> expr <* symbol(")"))
        )
    private lazy val rPair: Parsley[RValue] = RValue.RPair(pairElem)
    private lazy val rCall: Parsley[RValue] = 
        RValue.RCall(
            (symbol("call") *> ident),
            (symbol("(") *> option(argList) <* symbol(")"))
        )
    
    // Argument list definition
    private lazy val argList: Parsley[List[Expr]] = sepBy1(expr, symbol(","))

    // Pair elem definition
    private lazy val pairElem: Parsley[PairElem] = fstElem <|> sndElem

    private lazy val fstElem: Parsley[PairElem] =
        PairElem.FstElem(symbol("fst") *> lValue)
    private lazy val sndElem: Parsley[PairElem] =
        PairElem.SndElem(symbol("snd") *> lValue)

    // ArrayLiter definition
    private lazy val arrayLiter: Parsley[ArrayLiter] =
        ArrayLiter(symbol("[") *> option(argList) <* symbol("]"))
}
