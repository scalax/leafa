package net.scalax.leafa.slickimpl

import slick.ast.Select
import slick.jdbc.H2Profile.api._
import slick.jdbc.{JdbcProfile, SetParameter}

import scala.concurrent.{duration, Await, Future}

case class Friends(
  id: Option[Long] = None,
  name: String,
  nick: String,
  age: Int
)

class FriendTable(tag: slick.lifted.Tag) extends Table[Friends](tag, "friend") {

  def id   = column[Long]("id", O.AutoInc)
  def name = column[String]("name")
  def nick = column[String]("nick")
  def age  = column[Int]("age")

  def * = (id.?, name, nick, age).mapTo[Friends]

  def abc[T](rep: Rep[T])(implicit set: SetParameter[T]) = {
    BaseTypedTypeAst(rep.asInstanceOf[Rep.TypedRep[T]].toNode.asInstanceOf[Select].field.name, true)
  }

  lazy val columnAsts = List(
    abc(id),
    abc(name),
    abc(nick),
    abc(age)
  )

}

object SqlGen extends App {

  def await[A](f: Future[A]): A = Await.result(f, duration.Duration.Inf)

  val friendTq = TableQuery[FriendTable]

  val db = Database.forURL(s"jdbc:h2:mem:leafaaa;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver", keepAliveConnection = true)

  await(db.run(friendTq.schema.create))

  val friendTable = TableQuery[FriendTable].baseTableRow

  val tableInsert = {
    new SimpleInsert {
      override val typedTypeAstList = friendTable.columnAsts
      override val tableName        = friendTable.tableNode
      override val profile          = implicitly[JdbcProfile]
    }
  }

  //LeftData(insert into firend (id,name,nick,age) values (23,'sdfsaetgrest','fsdtgsertyert',98),List())
  await(
    db.run(
      tableInsert.takeColumn(List(23L, "sdfsaetgrest", "fsdtgsertyert", 98)).sql.asUpdate
    )
  )

  await(
    db.run(
      tableInsert.takeColumn(List(23L, "sdfsaetgrest", "fsdtgsertyert", 98)).sql.asUpdate
    )
  )

  await(
    db.run(
      tableInsert.takeColumn(List(23L, "sdfsaetgrest", "fsdtgsertyert", 98)).sql.asUpdate
    )
  )

  println(
    await(
      db.run(friendTable.result)
    )
  )

}
