package net.scalax.leafa.slickimpl

import net.scalax.asuna.slick.umr.rmu
import net.scalax.asuna.slick.umr.rmu.ExtMethod
import net.scalax.{leafa => baseLeafa}
import slick.ast._
import slick.jdbc._
import slick.lifted.Rep

import scala.language.implicitConversions

case class LeftData(sql: SQLActionBuilder)

trait SlickDBIOAstBase extends baseLeafa.BaseAst {

  type DataType
  def takeColumn(list: DataType): LeftData

}

trait SlickDBIOAst[T] extends baseLeafa.BaseAst with SlickDBIOAstBase {

  override type DataType = T
  override def takeColumn(list: T): LeftData

}

trait BlockAst[T] extends baseLeafa.BlockAst with SlickDBIOAst[T] {

  import net.scalax.asuna.slick.umr.rmu.SimpleSlickHelper._

  override val content: SlickDBIOAst[T]

  override def takeColumn(list: T): LeftData = {
    val oldSql = content.takeColumn(list)
    LeftData(sql = sql"""(""" concat oldSql.sql concat sql""")""")
  }

}

trait SqlColumnBase {
  import net.scalax.asuna.slick.umr.rmu.SimpleSlickHelper._

  type DataType
  val columnName: String
  def lawDataToSql(list: DataType): LeftData
  def dataToSql(list: DataType): LeftData = LeftData(sql"""(""" concat lawDataToSql(list).sql concat sql""")""")
}

trait SqlColumn[T] extends SqlColumnBase {
  override type DataType = T
  override def lawDataToSql(list: T): LeftData
  override def dataToSql(list: T): LeftData = super.dataToSql(list)
}

object SqlColumn {
  def apply[R](colName: String, tran: R => LeftData): SqlColumn[R] = new SqlColumn[R] {
    override def lawDataToSql(list: R): LeftData = tran(list)
    override val columnName                      = colName
  }

  implicit def extMethods[T](col: SqlColumn[T]): ExtMethod.ExtMoethods[T] = new rmu.ExtMethod.ExtMoethods[T] {
    override val base: SqlColumn[T] = col
  }
}

trait BaseTypedTypeAstBase extends baseLeafa.BaseTypedTypeAst with SqlColumnBase {

  override type DataType

  override val typedtype: Type
  override val isParam: Boolean

  val columnName: String

  override def lawDataToSql(list: DataType): LeftData = {
    LeftData(ExtMethod.customSet(t = typedtype, data = list, isParam = isParam))
  }

  override def dataToSql(list: DataType): LeftData = lawDataToSql(list)

}

trait BaseTypedTypeAst[T] extends baseLeafa.BaseTypedTypeAst with BaseTypedTypeAstBase with SqlColumn[T] {

  override type DataType = T
  override val typedtype: Type
  override val columnName: String

  override def dataToSql(list: T): LeftData = super.dataToSql(list)

}

object BaseTypedTypeAst {

  def apply[T](rep: Rep[T]): BaseTypedTypeAst[T] = {
    rep.toNode match {
      case Select(_, fieldSymbol @ FieldSymbol(name)) =>
        new BaseTypedTypeAst[T] {
          override val typedtype  = fieldSymbol.tpe
          override val isParam    = true
          override val columnName = name
        }
      case OptionApply(Select(_, fieldSymbol @ FieldSymbol(name))) =>
        new BaseTypedTypeAst[T] {
          override val typedtype  = fieldSymbol.tpe.asInstanceOf[TypedType[_]].optionType
          override val isParam    = true
          override val columnName = name
        }
    }
  }

}

trait SimpleInsert extends baseLeafa.SimpleInsert with SlickDBIOAst[List[Any]] {

  import net.scalax.asuna.slick.umr.rmu.SimpleSlickHelper._

  val typedTypeAstList: List[SqlColumnBase]

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
          item.lawDataToSql(currentData.asInstanceOf[item.DataType]).sql concat sql""", """
        } else {
          item.lawDataToSql(currentData.asInstanceOf[item.DataType]).sql
        }
        (action concat newAction, index + 1, leftData)
    }

    LeftData(sql"""insert into #${profile.quoteTableName(tableName)} (""" concat columnSql concat sql""") values (""" concat resultSql concat sql""")""")
  }

}
