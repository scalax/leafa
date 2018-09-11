package net.scalax.leafa.slickimpl

import slick.lifted.Rep
import net.scalax.{ leafa => baseLeafa }
import slick.ast.{ BaseTypedType, OptionTypedType, Select, TypedType }
import slick.jdbc.JdbcType

import scala.annotation.tailrec

case class LeftData(sql: String, left: List[Any])

trait SlickDBIOAstBase extends baseLeafa.BaseAst {

  type DataType
  def takeColumn(list: DataType, params: List[Any]): LeftData
  def addBaseTypedType(list: List[TypedType[_]]): List[TypedType[_]]

}

trait SlickDBIOAst[T] extends baseLeafa.BaseAst with SlickDBIOAstBase {

  override type DataType = T
  override def takeColumn(list: T, params: List[Any]): LeftData
  override def addBaseTypedType(list: List[TypedType[_]]): List[TypedType[_]] = list

}

trait BlockAst[T] extends baseLeafa.BlockAst with SlickDBIOAst[T] {

  override val content: SlickDBIOAst[T]

  override def takeColumn(list: T, params: List[Any]): LeftData = {
    val oldSql = content.takeColumn(list, params)
    LeftData(sql = "(" + oldSql.sql + ")", left = oldSql.left)
  }
  override def addBaseTypedType(list: List[TypedType[_]]): List[TypedType[_]] = content.addBaseTypedType(list)

}

trait BaseTypedTypeAstBase extends baseLeafa.BaseTypedTypeAst with SlickDBIOAstBase {

  override type DataType
  override val typedtype: Rep[DataType]
  override val isParam: Boolean

  override def takeColumn(list: DataType, params: List[Any]): LeftData = {
    val tpe = typedtype.asInstanceOf[Rep.TypedRep[DataType]].tpe
    if (!isParam) {
      @tailrec
      def withTypedType(typedtype: TypedType[_], data: Any): LeftData = {
        typedtype match {
          case baseTypedType: BaseTypedType[_] =>
            val jdbcTypedType = baseTypedType.asInstanceOf[JdbcType[Any]]
            LeftData(jdbcTypedType.valueToSQLLiteral(data), params)
          case optTypedType: OptionTypedType[_] =>
            data match {
              case Some(innerData) =>
                withTypedType(optTypedType.elementType, innerData)
              case _ => LeftData("null", params)
            }
        }
      }
      withTypedType(tpe, list)
    } else {
      LeftData("?", list :: params)
    }
  }
  override def addBaseTypedType(list: List[TypedType[_]]): List[TypedType[_]] = typedtype.asInstanceOf[Rep.TypedRep[DataType]].tpe :: list

}

trait BaseTypedTypeAst[T] extends baseLeafa.BaseTypedTypeAst with BaseTypedTypeAstBase with SlickDBIOAst[T] {

  override type DataType = T
  override val typedtype: Rep[DataType]

  override def takeColumn(list: T, params: List[Any]): LeftData = super.takeColumn(list, params)
  override def addBaseTypedType(list: List[TypedType[_]]): List[TypedType[_]] = super.addBaseTypedType(list)

}

object BaseTypedTypeAst {
  def apply[T](rep: Rep[T], isParam: Boolean = false): BaseTypedTypeAst[T] = {
    val rep1 = rep
    val isParam1 = isParam
    new BaseTypedTypeAst[T] {
      override val typedtype = rep1
      override val isParam = isParam1
    }
  }
}

trait SimpleInsert extends baseLeafa.SimpleInsert with SlickDBIOAst[List[Any]] {

  override val typedTypeAstList: List[BaseTypedTypeAstBase]
  val tableName: String

  override def takeColumn(lawData: List[Any], params: List[Any]): LeftData = {
    val tpeList = typedTypeAstList.map(t => t.typedtype)
    val columnNames = tpeList.map { s =>
      s.toNode match {
        case Select(_, symbol) => symbol.name
      }
    }
    val (values, leftLawData, left) = typedTypeAstList.foldLeft((List.empty[String], lawData, params)) {
      case ((currentValues, newLawData, leftParam), typedTypeBase) =>
        val LeftData(sql, newParams) = typedTypeBase.takeColumn(newLawData.head.asInstanceOf[typedTypeBase.DataType], leftParam)
        (sql :: currentValues, newLawData.tail, newParams)
    }
    LeftData(sql = s"""insert into ${tableName} (${columnNames.mkString(",")}) values (${values.reverse.mkString(",")})""", left = left)
  }
  override def addBaseTypedType(list: List[TypedType[_]]): List[TypedType[_]] = {
    typedTypeAstList.foldLeft(list) { (left, item) =>
      val newData = item.addBaseTypedType(left)
      newData
    }
  }

}