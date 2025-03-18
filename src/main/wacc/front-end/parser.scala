package wacc

import parsley.{Parsley}
import parsley.expr.{precedence, Ops, InfixL, InfixN, InfixR, Prefix}
import parsley.quick.*

// for testing
import parsley.errors.ErrorBuilder
import parsley.errors.combinator.ErrorMethods

import lexer.{
    fully, intLiter, binaryLiter, octalLiter, hexaLiter, boolLiter, charLiter, 
    strLiter, pairLiter, ident
}

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
        BinaryLiteral(binaryLiter).label("binary value") <|>
        OctalLiteral(octalLiter).label("octal value") <|>
        HexaLiteral(hexaLiter).label("hexadecimal value") <|>
        IntLiteral(intLiter).label("integer value") <|>
        BoolLiteral(boolLiter).label("boolean value") <|>
        CharLiteral(charLiter).label("char literal") <|>
        StrLiteral(strLiter).label("string literal") <|>
        pairLiter.as(PairLiteral).label("null pair value") <|>
        atomic(arrayElem).label("array index") <|>
        Identifier(ident).label("variable or function identifier") <|>
        (softOp("(") *> expr <* softOp(")")
            .label("closing bracket").explain("missing closing bracket")
        )

    // Array element definition
    private lazy val arrayElem: Parsley[ArrayElem] =
        ArrayElem(ident, some(softOp("[") *> expr <* softOp("]")
            .label("closing bracket for array")))

    // Expression definition
    private lazy val expr: Parsley[Expr] = 
        precedence(atom.label("an expression value"))(
            Ops(Prefix)(unaryOps.map(op => (expr: Expr) => UnaryOp(op, expr))
                .label("unary operator")), // '!' | '-' | 'len' | 'ord' | 'chr'
            Ops(InfixL)(mulOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right))
                .label("multiplication operator")), // '*' | '/' | '%'
            Ops(InfixL)(addOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right))
                .label("addition operator")), // '+' | '-'
            Ops(InfixN)(relOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right))
                .label("relational operator")), // '>' | '>=' | '<' | '<='
            Ops(InfixN)(eqOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right))
                .label("equality operator")),  // '==' | '!='
            Ops(InfixR)(andOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right))
                .label("logical operator")), // '&&'
            Ops(InfixR)(orOps.map(op => (left: Expr, right: Expr) => BinaryOp(left, op, right))
                .label("logical operator"))   // '||'
        ).label("expression")
    
    // Types

    // Type definition
    private lazy val typeParser: Parsley[Type] =
        (atomic(arrayType) <|> pairType <|> baseType).label("type") 

    // Base type definition
    private lazy val baseType: Parsley[BaseType] = 
        choice(
            symbol("int").as(BaseType.IntType),
            symbol("bin").as(BaseType.BinType),
            symbol("oct").as(BaseType.OctType),
            symbol("hex").as(BaseType.HexType),
            symbol("bool").as(BaseType.BoolType),
            symbol("char").as(BaseType.CharType),
            symbol("string").as(BaseType.StrType)
        )

    // Array type definition
    private lazy val arrayType: Parsley[ArrayType] =
        (pairType <|> baseType).flatMap { innerType =>
            // Match one or more occurrences of []
            some(softOp("[") *> softOp("]").label("closing bracket for array")).map { brackets =>
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
            (softOp(",") *> pairElemType <* softOp(")").label("closing bracket for pair"))
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
                stmt.filter(returningBlock).label("function body") <* 
            symbol("end").label("end for function"))
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
    private lazy val paramList: Parsley[List[Param]] = sepBy1(param, softOp(",").label("comma in parameters"))
    
    // Param definition
    private lazy val param: Parsley[Param] = Param(typeParser, ident)

    // Statement definition
    private lazy val stmtAtom: Parsley[Stmt] =
        symbol("skip").as(SkipStmt).label("skip statement") <|>
        DeclAssignStmt(typeParser, ident, (softOp("=") *> rValue))
            .label("declaration") <|>
        AssignStmt(lValue, (softOp("=") *> rValue)).label("assignment") <|>
        ReadStmt(symbol("read") *> lValue).label("read statement") <|>
        FreeStmt(symbol("free") *> expr).label("free statement") <|>
        PrintStmt(symbol("print") *> expr).label("print statement") <|>
        PrintlnStmt(symbol("println") *> expr).label("println statement") <|>
        atomic(returningStmt).label("returning statement") <|>
        IfStmt(
            (symbol("if") *> expr), 
            (symbol("then") *> stmt), 
            (symbol("else") *> stmt <* symbol("fi"))
        ).label("if-else statement") <|>
        WhileStmt(
            (symbol("while") *> expr),
            (symbol("do") *> stmt <* symbol("done"))
        ).label("while statement") <|>
        BodyStmt(symbol("begin") *> stmt <* symbol("end")).label("block statement")
    
    private lazy val stmt: Parsley[Stmt] = 
        precedence(stmtAtom.label("statement"))(
            Ops(InfixL)(SeqStmt from symbol(";").label("statement separator"))
        )

    // Left value definition
    private lazy val lValue: Parsley[LValue] = 
        (atomic(lArray) <|> lName <|> lPair).label("left-side value")

    private lazy val lName: Parsley[LValue] = LValue.LName(ident).label("identifier")
    private lazy val lArray: Parsley[LValue] = LValue.LArray(arrayElem).label("array index")
    private lazy val lPair: Parsley[LValue] = LValue.LPair(pairElem).label("pair")

    // Right value definition
    private lazy val rValue: Parsley[RValue] =
        (rExpr <|> 
        rArrayLiter <|>
        rNewPair <|>
        rPair <|>
        rCall).label("right-side value")

    private lazy val rExpr: Parsley[RValue] = RValue.RExpr(expr).label("expression")
    private lazy val rArrayLiter: Parsley[RValue] = RValue.RArrayLiter(arrayLiter).label("array")
    private lazy val rNewPair: Parsley[RValue] = 
        RValue.RNewPair(
            (symbol("newpair") *> softOp("(") *> expr),
            (softOp(",") *> expr <* softOp(")"))
        ).label("new pair")
    private lazy val rPair: Parsley[RValue] = RValue.RPair(pairElem).label("pair")
    private lazy val rCall: Parsley[RValue] = 
        RValue.RCall(
            (symbol("call") *> ident),
            (softOp("(") *> option(argList) <* softOp(")"))
        ).label("function call")
    
    // Argument list definition
    private lazy val argList: Parsley[List[Expr]] = sepBy1(expr, softOp(",")).label("argument list")

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
