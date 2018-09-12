package net.scalax.leafa

import slick.ast.TableNode

trait BaseAst

trait BlockAst extends BaseAst {
  val content: BaseAst
}

trait BaseTypedTypeAst extends BaseAst {

  type DataType
  val isParam: Boolean

}

trait SimpleInsert extends BaseAst {

  val typedTypeAstList: List[BaseTypedTypeAst]
  val tableName: TableNode

}
