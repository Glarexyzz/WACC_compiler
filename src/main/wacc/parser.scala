package wacc

import parsley.{Parsley}
import parsley.expr.{precedence, Ops, InfixL, InfixN, InfixR, Prefix}
import parsley.quick.*

// for testing
import parsley.errors.ErrorBuilder
import parsley.errors.combinator.ErrorMethods

import lexer.{fully, intLiter, boolLiter, charLiter, strLiter, pairLiter, ident}

object parser {
    def parse[Err >: ParserError: ErrorBuilder](prog: String): Either[Err, Any] = {
        fully(program).parse(prog) match {
            case parsley.Success(result) => 
                Right(result)

            case parsley.Failure(err) => 
                Left(err)
        }
    }

    private lazy val symbol = lexer.lexeme.symbol  // shorten for hardKeyword & hardOps
    val softOp = lexer.lexeme.symbol.softOperator  // shorten for softOps

    // Expression parsers

    // Unary Operators (Highest Precedence)
    private lazy val unaryOps: Parsley[UnaryOperator] =
        choice(
            symbol("!").as(UnaryOperator.Not),
            atomic(symbol("-") <* notFollowedBy(digit)).as(UnaryOperator.Negate),
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
        atomic(arrayElem) <|>
        Identifier(ident) <|>
        (softOp("(") *> expr <* softOp(")").label("closing bracket").explain("missing closing bracket"))

    // Array element definition
    private lazy val arrayElem: Parsley[ArrayElem] =
        ArrayElem(ident, some(softOp("[") *> expr <* softOp("]")))

    // Expression definition
    private lazy val expr: Parsley[Expr] = 
        precedence(atom)(
            Ops(Prefix)(unaryOps.map(op => (expr: Expr) => UnaryOp(op, expr))), // '!' | '-' | 'len' | 'ord' | 'chr'
            Ops(InfixL)(mulOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right))), // '*' | '/' | '%'
            Ops(InfixL)(addOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right))), // '+' | '-'
            Ops(InfixN)(relOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right))), // '>' | '>=' | '<' | '<='
            Ops(InfixN)(eqOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right))),  // '==' | '!='
            Ops(InfixR)(andOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right))), // '&&'
            Ops(InfixR)(orOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right)))   // '||'
        )
    
    // Types

    // Type definition
    private lazy val typeParser: Parsley[Type] =
        atomic(arrayType) <|> pairType <|> baseType 

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
        (pairType <|> baseType).flatMap { innerType =>
            // Match one or more occurrences of []
            some(softOp("[") *> softOp("]")).map { brackets =>
                // Start with innerType, and wrap it for each "[]"
                brackets.foldLeft(innerType) { (acc, _) =>
                    // Ensure that the accumulator is always wrapped in ArrayType
                    ArrayType(acc)  // Safely cast to ArrayType
                }.asInstanceOf[ArrayType]
            }
        }

    // Pair definition
    private lazy val pairType: Parsley[PairType] = 
        PairType(
            (symbol("pair") *> softOp("(") *> pairElemType), 
            (softOp(",") *> pairElemType <* softOp(")"))
        )

    // Pair element type definition
    private lazy val pairElemType: Parsley[PairElemType] = 
        pairKeyword <|> atomic(arrayTElem) <|> baseTElem

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
            (symbol("begin") *> many(atomic(func))), 
            (stmt <* symbol("end"))
        )

    // Func definition
    private lazy val func: Parsley[Func] = 
        Func(  
            typeParser, 
            ident, 
            (softOp("(") *> option(paramList) <* softOp(")")),
            (symbol("is") *> 
                stmt.filter(returningBlock) <* 
            symbol("end"))
        )
    
    // Returning statement definition
    private lazy val returningStmt: Parsley[Stmt] =
        returnStmt <|> exitStmt <|> returningIfStmt
    
    private lazy val returnStmt: Parsley[Stmt] =
        ReturnStmt(symbol("return") *> expr)

    private lazy val exitStmt: Parsley[Stmt] =
        ExitStmt(symbol("exit") *> expr)

    private lazy val returningIfStmt: Parsley[Stmt] =
        IfStmt(
            (symbol("if") *> expr), 
            (symbol("then") *> returningStmt), 
            (symbol("else") *> returningStmt <* symbol("fi"))
        )

    private def returningBlock(s: Stmt): Boolean = s match {
        case ReturnStmt(_) | ExitStmt(_) => true  //  Return/Exit is valid
        case IfStmt(_, thenStmt, elseStmt) => 
            returningBlock(thenStmt) && returningBlock(elseStmt) 
        case SeqStmt(_, lastStmt) => returningBlock(lastStmt) 
        case BodyStmt(innerStmt) => returningBlock(innerStmt)
        case _ => false  // Any other statement is invalid
    }

    // ParamList definition
    private lazy val paramList: Parsley[List[Param]] = sepBy1(param, softOp(","))
    
    // Param definition
    private lazy val param: Parsley[Param] = Param(typeParser, ident)

    // Statement definition
    private lazy val stmtAtom: Parsley[Stmt] =
        symbol("skip").as(SkipStmt) <|>
        DeclAssignStmt(typeParser, ident, (softOp("=") *> rValue)) <|>
        AssignStmt(lValue, (softOp("=") *> rValue)) <|>
        ReadStmt(symbol("read") *> lValue) <|>
        FreeStmt(symbol("free") *> expr) <|>
        PrintStmt(symbol("print") *> expr) <|>
        PrintlnStmt(symbol("println") *> expr) <|>
        atomic(returningStmt) <|>
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
        atomic(lArray) <|> lName <|> lPair

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
            (symbol("newpair") *> softOp("(") *> expr),
            (softOp(",") *> expr <* softOp(")"))
        )
    private lazy val rPair: Parsley[RValue] = RValue.RPair(pairElem)
    private lazy val rCall: Parsley[RValue] = 
        RValue.RCall(
            (symbol("call") *> ident),
            (softOp("(") *> option(argList) <* softOp(")"))
        )
    
    // Argument list definition
    private lazy val argList: Parsley[List[Expr]] = sepBy1(expr, softOp(","))

    // Pair elem definition
    private lazy val pairElem: Parsley[PairElem] = fstElem <|> sndElem

    private lazy val fstElem: Parsley[PairElem] =
        PairElem.FstElem(symbol("fst") *> lValue)
    private lazy val sndElem: Parsley[PairElem] =
        PairElem.SndElem(symbol("snd") *> lValue)

    // ArrayLiter definition
    private lazy val arrayLiter: Parsley[ArrayLiter] =
        ArrayLiter(softOp("[") *> option(argList) <* softOp("]"))
}
