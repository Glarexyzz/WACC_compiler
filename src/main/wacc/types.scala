package wacc

import parsley.generic

sealed trait Type

enum BaseType extends Type {
    case IntType
    case BoolType
    case CharType
    case StrType
}

case object AnyType extends Type

// <type> '[' ']'
case class ArrayType(innerType: Type) extends Type
object ArrayType extends generic.ParserBridge1[Type, ArrayType]

// 'pair' '(' <pairElemType> ',' <pairElemType> ')'
case class PairType(
    leftElem: PairElemType, 
    rightElem: PairElemType
) extends Type
object PairType extends generic.ParserBridge2[
    PairElemType, PairElemType, PairType
]

// <baseType> | <arrayType> | 'pair'
sealed trait PairElemType extends Type

case class BaseTElem(elem: BaseType) extends PairElemType
object BaseTElem extends generic.ParserBridge1[BaseType, BaseTElem]
case class ArrayTElem(elem: ArrayType) extends PairElemType
object ArrayTElem extends generic.ParserBridge1[ArrayType, ArrayTElem]
case object PairKeyword extends PairElemType // 'pair' keyword
case object NullType extends PairElemType // 'null' keyword