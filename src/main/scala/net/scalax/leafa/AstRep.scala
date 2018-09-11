package net.scalax.leafa

import slick.lifted.Rep

trait BaseAst

trait BlockAst extends BaseAst {
  val content: BaseAst
}

trait BaseTypedTypeAst extends BaseAst {

  type DataType
  val typedtype: Rep[DataType]
  val isParam: Boolean

}

trait SimpleInsert extends BaseAst {

  val typedTypeAstList: List[BaseTypedTypeAst]

}