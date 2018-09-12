package net.scalax.leafa.slickimpl

import net.scalax.{leafa => baseLeafa}
import slick.ast.TableNode
import slick.jdbc._

import scala.language.implicitConversions

case class LeftData(sql: SQLActionBuilder)

trait SlickHelper {
  implicit def actionBasedSQLInterpolation(s: StringContext): ActionBasedSQLInterpolation = new ActionBasedSQLInterpolation(s)

  implicit class asfjawhterhtierhnt(a: SQLActionBuilder) {
    def concat(b: SQLActionBuilder): SQLActionBuilder = {
      SQLActionBuilder(a.queryParts ++ b.queryParts, new SetParameter[Unit] {
        def apply(p: Unit, pp: PositionedParameters): Unit = {
          a.unitPConv.apply(p, pp)
          b.unitPConv.apply(p, pp)
        }
      })
    }
  }
}

object SlickHelper extends SlickHelper

trait SlickDBIOAstBase extends baseLeafa.BaseAst {

  type DataType
  def takeColumn(list: DataType): LeftData

}

trait SlickDBIOAst[T] extends baseLeafa.BaseAst with SlickDBIOAstBase {

  override type DataType = T
  override def takeColumn(list: T): LeftData

}

trait BlockAst[T] extends baseLeafa.BlockAst with SlickDBIOAst[T] {

  import SlickHelper._

  override val content: SlickDBIOAst[T]

  override def takeColumn(list: T): LeftData = {
    val oldSql = content.takeColumn(list)
    LeftData(sql = sql"""(""" concat oldSql.sql concat sql""")""")
  }

}

trait BaseTypedTypeAstBase extends baseLeafa.BaseTypedTypeAst with SlickDBIOAstBase {

  import SlickHelper._

  override type DataType
  val typedtype: SetParameter[DataType]
  val columnName: String
  override val isParam: Boolean

  override def takeColumn(list: DataType): LeftData = {
    implicit val typedType1 = typedtype
    LeftData(sql"""${list}""")
  }

}

trait BaseTypedTypeAst[T] extends baseLeafa.BaseTypedTypeAst with BaseTypedTypeAstBase with SlickDBIOAst[T] {

  override type DataType = T
  override val typedtype: SetParameter[DataType]
  override val columnName: String

  override def takeColumn(list: T): LeftData = super.takeColumn(list)

}

object BaseTypedTypeAst {

  def apply[T](columnName: String, isParam: Boolean = false)(implicit rep: SetParameter[T]): BaseTypedTypeAst[T] = {
    val rep1        = rep
    val isParam1    = isParam
    val columnName1 = columnName
    new BaseTypedTypeAst[T] {
      override val typedtype  = rep1
      override val isParam    = isParam1
      override val columnName = columnName1
    }
  }

}

trait SimpleInsert extends baseLeafa.SimpleInsert with SlickDBIOAst[List[Any]] {

  import SlickHelper._

  override val typedTypeAstList: List[BaseTypedTypeAstBase]

  val profile: JdbcProfile

  override val tableName: TableNode

  override def takeColumn(lawData: List[Any]): LeftData = {
    val columnSize = typedTypeAstList.size
    val (columnSql, _) = typedTypeAstList.foldLeft((sql"""""", 0)) {
      case ((action, index), item) =>
        val newAction = if (index < columnSize - 1) {
          sql"""#${profile.quoteIdentifier(item.columnName)}, """
        } else {
          sql"""#${profile.quoteIdentifier(item.columnName)}"""
        }
        (action concat newAction, index + 1)
    }

    val (resultSql, _, _) = typedTypeAstList.foldLeft((sql"""""", 0, lawData)) {
      case ((action, index, currentData :: leftData), item) =>
        val newAction = if (index < columnSize - 1) {
          item.takeColumn(currentData.asInstanceOf[item.DataType]).sql concat sql""", """
        } else {
          item.takeColumn(currentData.asInstanceOf[item.DataType]).sql
        }
        (action concat newAction, index + 1, leftData)
    }

    LeftData(sql"""insert into #${profile.quoteTableName(tableName)} (""" concat columnSql concat sql""") values (""" concat resultSql concat sql""")""")
  }

}
