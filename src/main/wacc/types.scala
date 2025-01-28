package wacc

import parsley.generic
import parsley.Parsley

sealed trait Type

enum BaseType extends Type {
    case IntType
    case BoolType
    case CharType
    case StrType
}

case class ArrayType(innerType: Type) extends Type

case class PairType(
    leftElem: PairElemType, 
    rightElem: PairElemType
) extends Type
object PairType extends generic.ParserBridge2[
    PairElemType, PairElemType, PairType
]

sealed trait PairElemType

case class BaseTElem(elem: BaseType) extends PairElemType
case class ArrayTElem(elem: ArrayType) extends PairElemType
case object PairElem extends PairElemType // 'pair' keyword