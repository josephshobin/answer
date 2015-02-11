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

import java.math.{BigDecimal => JBigDecimal}
import java.sql.{Date, Time, Timestamp}

import org.joda.time.{DateTime, LocalDate, LocalDateTime, LocalTime}

import scalikejdbc._

import org.specs2.Specification
import org.specs2.matcher.{TerminationMatchers, ThrownExpectations}

object ExtractorMacroSpec extends Specification
    with TerminationMatchers
    with ThrownExpectations { def is = sequential ^ s2"""

ExtractorMacro
==============

  Can automatically create an extractor for a row $extract

"""

  def extract = {
    Class.forName("org.hsqldb.jdbcDriver")
    ConnectionPool.singleton("jdbc:hsqldb:mem:test", "sa", "")

    DB.autoCommit { implicit session =>
      sql"""CREATE SCHEMA test AUTHORIZATION DBA""".execute.apply()
      sql"""DROP TABLE IF EXISTS test.test""".execute.apply()
      sql"""CREATE TABLE test.test (
          booleanColumn        boolean,
          byteColumn           tinyint,
          doubleColumn         double,
          floatColumn          float,
          intColumn            integer,
          longColumn           bigint,
          stringColumn         varchar(1024),
          bigDecimalColumn     decimal(12, 0),
          dateColumn           date,
          jDateTimeColumn      timestamp,
          jLocalDateColumn     date,
          jLocalDateTimeColumn timestamp,
          jLocalTimeColumn     time,
          timeColumn           time,
          timestampColumn      timestamp
        )""".execute.apply()

      val data = List(
        (
          Option(true), Option(1), Option(3.0), Option(4.0F), Option(6), Option(8L), Option("abc"),
          Option(new JBigDecimal(30)), Option(new Date(493567200000L)),
          Option(new DateTime(493567200000L)), Option(new LocalDate(493567200000L)),
          Option(new LocalDateTime(493567200000L)), Option(new LocalTime(18000000L)),
          Option(new Time(18000000L)), Option(new Timestamp(493567200000L))
        ), (None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)
      )

      data.map(d => 
        sql"""INSERT INTO test.test VALUES((
          ${d._1}, ${d._2}, ${d._3}, ${d._4}, ${d._5}, ${d._6}, ${d._7},
          ${d._8}, ${d._9}, ${d._10}, ${d._11}, ${d._12}, ${d._13}, ${d._14},
          ${d._15}
        ))""".execute.apply
      )

      val ex = ExtractorMacro.mkExtractor[(
        Option[Boolean], Option[Byte], Option[Double], Option[Float], Option[Int], Option[Long],
        Option[String], Option[JBigDecimal], Option[Date], Option[DateTime], Option[LocalDate],
        Option[LocalDateTime], Option[LocalTime], Option[Time], Option[Timestamp]
      )]

      val result1 = sql"SELECT * FROM test.test WHERE booleanColumn IS NOT NULL".map(ex.extract).single.apply()
      val result2 = sql"SELECT * FROM test.test WHERE booleanColumn IS NULL".map(ex.extract).single.apply()

      result1.get._1 must_== data.head._1
      result1.get._2 must_== data.head._2
      result1.get._3 must_== data.head._3
      result1.get._4 must_== data.head._4
      result1.get._5 must_== data.head._5
      result1.get._6 must_== data.head._6
      result1.get._7 must_== data.head._7
      result1.get._8 must_== data.head._8
      result1.get._9 must_== data.head._9
      result1.get._10 must_== data.head._10
      result1.get._11 must_== data.head._11
      result1.get._12 must_== data.head._12
      result1.get._13 must_== data.head._13
      result1.get._14 must_== data.head._14
      result1.get._15 must_== data.head._15

      result1 must_== Option(data.head)
      result2 must_== Option(data.last)
    }
  }
}
