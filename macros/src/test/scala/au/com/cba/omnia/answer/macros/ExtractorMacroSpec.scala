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

import java.sql.{Date, Time, Timestamp}

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
          bigDecimalColumn     decimal(12, 4),
          dateColumn           date,
          jDateTimeColumn      timestamp,
          jLocalDateColumn     date,
          jLocalDateTimeColumn timestamp,
          jLocalTimeColumn     time,
          timeColumn           time,
          timestampColumn      timestamp
        )""".execute.apply()

      val data = (
        true, 1, 3.0, 4.0F, 6, 8L, "abc",
        BigDecimal(30), new Date(493567200000L), new Timestamp(493567200000L),
        new Date(493567200000L), new Timestamp(493567200000L), new Time(493567200000L),
        new Time(493567200000L), new Timestamp(493567200000L)
      )

      sql"""INSERT INTO test.test VALUES((
          ${data._1}, ${data._2}, ${data._3}, ${data._4}, ${data._5}, ${data._6}, ${data._7},
          ${data._8}, ${data._9}, ${data._10}, ${data._11}, ${data._12}, ${data._13}, ${data._14},
          ${data._15}
        ))""".execute.apply

      val ex = ExtractorMacro.mkExtractor[Date]
      sql"SELECT dateColumn FROM test.test".map(ex.extract).single.apply() must_== Option(data._9)
    }
  }
}
