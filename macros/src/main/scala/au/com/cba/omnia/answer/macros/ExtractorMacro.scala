//   Copyright 2014 Commonwealth Bank of Australia
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

package au.com.cba.omnia.answer.macros

import scala.reflect.macros.Context

import scalikejdbc.{DB => SDB, _}

case class Extractor[A](extract: WrappedResultSet => A)

object ExtractorMacro {
  def mkExtractor[A]: Extractor[A] = macro impl[A]

  def impl[A : c.WeakTypeTag](c: Context): c.Expr[Extractor[A]] = {
    import c.universe._

    val typ = c.universe.weakTypeOf[A]

    /** Fail compilation with nice error message. */
    def abort(msg: String) =
      c.abort(c.enclosingPosition, s"Can't create Extractor for $typ: $msg")


    // Primitives plus String
    val booleanType           = c.universe.weakTypeOf[Boolean]
    val byteType              = c.universe.weakTypeOf[Byte]
    val doubleType            = c.universe.weakTypeOf[Double]
    val floatType             = c.universe.weakTypeOf[Float]
    val intType               = c.universe.weakTypeOf[Int]
    val longType              = c.universe.weakTypeOf[Long]
    val stringType            = c.universe.weakTypeOf[String]

    val bigDecimalType        = c.universe.weakTypeOf[java.math.BigDecimal]
    val dateType              = c.universe.weakTypeOf[java.sql.Date]
    val jodaDateTimeType      = c.universe.weakTypeOf[org.joda.time.DateTime]
    val jodaLocalDateType     = c.universe.weakTypeOf[org.joda.time.LocalDate]
    val jodaLocalDateTimeType = c.universe.weakTypeOf[org.joda.time.LocalDateTime]
    val jodaLocalTimeType     = c.universe.weakTypeOf[org.joda.time.LocalTime]
    val timeType              = c.universe.weakTypeOf[java.sql.Time]
    val timestampType         = c.universe.weakTypeOf[java.sql.Timestamp]

    typ match {
      case `booleanType`           => c.Expr[Extractor[A]](q"Extractor(rs => rs.boolean(1))")
      case `byteType`              => c.Expr[Extractor[A]](q"Extractor(rs => rs.byte(1))")
      case `doubleType`            => c.Expr[Extractor[A]](q"Extractor(rs => rs.double(1))")
      case `floatType`             => c.Expr[Extractor[A]](q"Extractor(rs => rs.float(1))")
      case `intType`               => c.Expr[Extractor[A]](q"Extractor(rs => rs.int(1))")
      case `longType`              => c.Expr[Extractor[A]](q"Extractor(rs => rs.long(1))")
      case `stringType`            => c.Expr[Extractor[A]](q"Extractor(rs => rs.string(1))")
      case `bigDecimalType`        => c.Expr[Extractor[A]](q"Extractor(rs => rs.bigDecimal(1))")
      case `dateType`              => c.Expr[Extractor[A]](q"Extractor(rs => rs.date(1))")
      case `jodaDateTimeType`      => c.Expr[Extractor[A]](q"Extractor(rs => rs.jodaDateTime(1))")
      case `jodaLocalDateType`     => c.Expr[Extractor[A]](q"Extractor(rs => rs.jodaLocalDate(1))")
      case `jodaLocalDateTimeType` => c.Expr[Extractor[A]](q"Extractor(rs => rs.jodaLocalDateTime(1))")
      case `jodaLocalTimeType`     => c.Expr[Extractor[A]](q"Extractor(rs => rs.jodaLocalTime(1))")
      case `timeType`              => c.Expr[Extractor[A]](q"Extractor(rs => rs.time(1))")
      case `timestampType`         => c.Expr[Extractor[A]](q"Extractor(rs => rs.timestamp(1))")
      case _                       => abort(s"$typ is not an expected type")
    }
  }
}


