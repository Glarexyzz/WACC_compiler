package wacc

import parsley.{Parsley, Result}
import parsley.expr.chain

import lexer.implicits.implicitSymbol
import lexer.{digit, fully}

object parser {
    def parse(input: String): Result[String, BigInt] = parser.parse(input)
    //private val parser = fully(expr)
    
    // Operators
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

    private val not = (x: Boolean) => !x
    private val negate = (x: BigInt) => -x
    /*
    private lazy val expr: Parsley[BigInt] =
        chain.left1(digit | "(" ~> expr <~ ")")(
            ("+" as add) | ("-" as sub)
        )
    */
}
