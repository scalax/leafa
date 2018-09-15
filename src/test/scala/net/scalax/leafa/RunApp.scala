package net.scalax.leafa.slickimpl

import net.scalax.asuna.slick.umr.rmu.{ExtMethod, RmuWriterQuery}
import slick.jdbc.H2Profile.api._

import scala.concurrent.{duration, Await, Future}

case class Friends(
    id: Option[Long] = None
  , name: String
  , nick: String
  , age: Int
)

class FriendTable(tag: slick.lifted.Tag) extends Table[Friends](tag, "friend") with RmuWriterQuery {
  self =>

  val ext = ExtMethod

  def id1      = column[Option[Long]]("id", O.AutoInc)
  def commonId = id1
  def id       = ext.liftCol(id1) ++ 3 - 6
  def name     = column[String]("name")
  def nick     = column[String]("nick")
  def age      = column[Int]("age")

  lazy val columnAsts = rmu.effect(rmu.caseOnly[FriendTable, Friends].compileEncoder2222.inputTable(self)).withCols(self.tableNode)

  def * = (commonId, name, nick, age).mapTo[Friends]

}

object SqlGen extends App {

  def await[A](f: Future[A]): A = Await.result(f, duration.Duration.Inf)

  val friendTq = TableQuery[FriendTable]

  val db = Database.forURL(s"jdbc:h2:mem:leafaaa;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver", keepAliveConnection = true)

  await(db.run(friendTq.schema.create))

  val friendTable = TableQuery[FriendTable].baseTableRow

  await(
      db.run(
        friendTable.columnAsts.inputData(Friends(Option(23L), "fsdtgsertyert", "sdfsaetgrest", 98))
    )
  )

  await(
      db.run(
        friendTable.columnAsts.inputData(Friends(Option(23L), "sdfsaetgrest", "fsdtgsertyert", 98))
    )
  )

  await(
      db.run(
        friendTable.columnAsts.inputData(Friends(Option.empty, "sdfsaetgrest", "fsdtgsertyert", 98))
    )
  )

  println(
      await(db.run(friendTq.result)).mkString("\n")
  )

}
