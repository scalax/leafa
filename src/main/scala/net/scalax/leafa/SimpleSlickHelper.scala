package net.scalax.asuna.slick.umr.rmu

import slick.jdbc._
import scala.language.implicitConversions
trait SimpleSlickHelper {
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

object SimpleSlickHelper extends SimpleSlickHelper
