package net.scalax.leafa.slickimpl

import slick.jdbc.H2Profile.api._

case class Friends(
  id: Option[Long] = None,
  name: String,
  nick: String,
  age: Int)

class FriendTable(tag: slick.lifted.Tag) extends Table[Friends](tag, "firend") {
  def id = column[Long]("id", O.AutoInc)
  def name = column[String]("name")
  def nick = column[String]("nick")
  def age = column[Int]("age")

  def * = (id.?, name, nick, age).mapTo[Friends]

  lazy val columnAsts = List(BaseTypedTypeAst(id), BaseTypedTypeAst(name, true), BaseTypedTypeAst(nick), BaseTypedTypeAst(age))

}

object SqlGen extends App {

  val friendTable = TableQuery[FriendTable].baseTableRow

  lazy val tableInsert = {
    val tableName1 = friendTable.tableName
    new SimpleInsert {
      override val typedTypeAstList = friendTable.columnAsts
      override val tableName: String = tableName1
    }
  }

  //LeftData(insert into firend (id,name,nick,age) values (23,'sdfsaetgrest','fsdtgsertyert',98),List())
  println(tableInsert.takeColumn(List(23L, "sdfsaetgrest", "fsdtgsertyert", 98), List.empty))

}