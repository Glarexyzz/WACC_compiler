package wacc

import parsley.{Success, Failure}

def main(args: Array[String]): Unit = {
    var exitCode = 0
    println("hello WACC!")

    args.headOption match {
        case Some(expr) => parser.parse(expr) match {
            case Success(x) => println(s"$expr = $x")
            case Failure(msg) => println(msg)
        }
        case None => println("please enter an expression")
    }
    println("pls exit WACC!")
    System.exit(exitCode)
}
