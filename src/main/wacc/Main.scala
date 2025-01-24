package wacc

import parsley.{Success, Failure}
import Constants._

def main(args: Array[String]): Unit = {
    var exitCode = exitValid
    println("hello WACC!") // REMOVE WHEN NO LONGER NEEDED

    // args.headOption match {
    //     case Some(expr) => parser.parse(expr) match {
    //         case Success(x) => println(s"$expr = $x")
    //         case Failure(msg) => println(msg)
    //     }
    //     case None => println("please enter an expression")
    // }
    println("pls exit WACC!")
    System.exit(exitCode)
}
