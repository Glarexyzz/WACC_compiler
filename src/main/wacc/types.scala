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


sealed trait PairElemType

case class BaseElem(elem: BaseType) extends PairElemType
case class ArrayElem(elem: ArrayType) extends PairElemType
case object PairElem extends PairElemType // 'pair' keyword