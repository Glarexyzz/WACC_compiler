package wacc

import parsley.errors.ErrorBuilder
import parsley.errors.Token

case class ParserError(pos: (Int, Int), source: String, lines: ParserErrorLines)


enum ParserErrorLines {
  case VanillaError(
      unexpected: Option[ParserErrorItem], 
      expecteds: Set[ParserErrorItem], 
      reasons: Set[String], 
      width: Int
  )
  case SpecialisedError(msgs: Set[String], width: Int)
}


sealed trait ParserErrorItem
case class ParserRaw(item: String) extends ParserErrorItem
case class ParserNamed(item: String) extends ParserErrorItem
case object ParserEndOfInput extends ParserErrorItem

class WACCErrorBuilder extends ErrorBuilder[ParserError] {
    // Type aliases
    type Position = (Int, Int)
    type Source = String
    type ErrorInfoLines = ParserErrorLines
    type Item = ParserErrorItem
    type Raw = ParserRaw
    type Named = ParserNamed
    type EndOfInput = ParserEndOfInput.type
    type Message = String
    type Messages = Set[String]
    type ExpectedItems = Set[ParserErrorItem]
    type ExpectedLine = Set[ParserErrorItem]
    type UnexpectedLine = Option[ParserErrorItem]
    type LineInfo = Int

  
    def build(pos: (Int, Int), source: String,
              lines: ParserErrorLines): ParserError = ParserError(pos, source, lines)
    
    def vanillaError(
        unexpected: Option[ParserErrorItem],
        expected: Set[ParserErrorItem],
        reasons: Set[String],
        width: Int
      ): ParserErrorLines = ParserErrorLines.VanillaError(unexpected, expected, reasons, width)
    
    def specializedError(
        msgs: Set[String],
        width: Int
      ): ParserErrorLines = ParserErrorLines.SpecialisedError(msgs, width)
    
    def pos(line: Int, col: Int): (Int, Int) = (line, col)
    def source(sourceName: Option[String]): String = sourceName.getOrElse("<input>")
    def combineExpectedItems(alts: Set[ParserErrorItem]): Set[ParserErrorItem] = alts
    def combineMessages(alts: Seq[String]): Set[String] = alts.toSet
    def unexpected(item: Option[ParserErrorItem]): Option[ParserErrorItem] = item
    def expected(alts: Set[ParserErrorItem]): Set[ParserErrorItem] = alts
    def message(msg: String): String = msg
    def reason(msg: String): String = msg
    def raw(item: String): ParserRaw = ParserRaw(item)
    def named(item: String): ParserNamed = ParserNamed(item)
    val endOfInput: ParserEndOfInput.type = ParserEndOfInput

    val numLinesAfter: Int = 0
    val numLinesBefore: Int = 0
    def lineInfo(
        line: String,
        linesBefore: Seq[String],
        linesAfter: Seq[String],
        lineNum: Int, errorPointsAt: Int, errorWidth: Int
      ): Int = errorWidth

    // The implementation of this is usually provided by a mixed-in
    // token extractor, discussed in `tokenextractors`
    def unexpectedToken(
        cs: Iterable[Char],
        amountOfInputParserWanted: Int,
        lexicalError: Boolean
      ): Token = ???
}

def generateSemanticError(pos: (Int, Int), unexpected: String, expected: String): ParserError = ???