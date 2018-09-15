package net.scalax.leafa

import slick.ast.{TableNode, Type}

trait BaseAst

trait BlockAst extends BaseAst {
  val content: BaseAst
}

trait BaseTypedTypeAst extends BaseAst {

  type DataType
  val typedtype: Type
  val isParam: Boolean

}

trait SimpleInsert extends BaseAst {

  val tableName: TableNode

}
