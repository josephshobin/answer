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

package au.com.cba.omnia.answer

import java.sql.Connection

import scalikejdbc.{DB => SDB, _}

import scalaz._, Scalaz._
import scalaz.\&/.{This, That}
import scalaz.scalacheck.ScalazProperties.monad

import org.specs2.{Specification, ScalaCheck}
import org.specs2.matcher.{TerminationMatchers, ThrownExpectations, Parameters, Matcher}
import org.specs2.execute.{Result => SpecResult}

import org.scalacheck.Arbitrary, Arbitrary.arbitrary

import au.com.cba.omnia.omnitool.{Result, Ok, Error}
import au.com.cba.omnia.omnitool.test.Arbitraries._

class DBSpec extends Specification
  with TerminationMatchers
  with ThrownExpectations
  with ScalaCheck { def is = s2"""
DB Operations
===============

DB operations should:
  obey monad laws                                         ${monad.laws[DB]}
  ||| is alias for `or`                                   $orAlias
  or stops at first succeess                              $orFirstOk
  or continues at first Error                             $orFirstError
  mandatory success iif result is true                    $mandatoryMeansTrue
  forbidden success iif result is false                   $forbiddenMeansFalse
  recoverWith for all cases is the same as |||            $recoverWith
  recoverWith only recovers the specified errors          $recoverWithSpecific
  bracket always executes `after` action                  $bracket

DB construction:
  result is constant                                      $result
  hdfs handles exceptions                                 $safeDB
  value handles exceptions                                $safeValue
  guard success iif condition is true                     $guardMeansTrue
  prevent success iif condition is false                  $preventMeansFalse

DB query:
  can ask for data                                        $ask
  can do a queryFirst                                     $queryFirst
  can do a querySingle                                    $querySingle
  can do a query for list                                 $query

"""

  Class.forName("org.hsqldb.jdbcDriver")
  ConnectionPool.singleton("jdbc:hsqldb:mem:test", "sa", "") 
  setupDb

  def connection: Connection =  {
    ConnectionPool.borrow()
  }

  def orAlias = prop((x: DB[Int], y: DB[Int]) =>
    (x ||| y).run(connection) must_== (x or y).run(connection))

  def orFirstOk = prop((x: Int, y: DB[Int]) =>
    (DB.result(Result.ok(x)) ||| y).run(connection) must_==
      DB.result(Result.ok(x)).run(connection))

  def orFirstError = prop((x: String, y: DB[Int]) =>
    (DB.fail(x) ||| y).run(connection) must_== y.run(connection))

  def mandatoryMeansTrue = prop((x: DB[Boolean], msg: String) => {
    val runit = DB.mandatory(x, msg).run(connection)
    val rbool = x.run(connection)
    (runit, rbool) must beLike {
      case (Ok(_), Ok(true))     => ok
      case (Error(_), Ok(false)) => ok
      case (Error(_), Error(_))  => ok
    }
  })

  def forbiddenMeansFalse = prop((x: DB[Boolean], msg: String) => {
    val runit = DB.forbidden(x, msg).run(connection)
    val rbool = x.run(connection)
    (runit, rbool) must beLike {
      case (Ok(_), Ok(false))   => ok
      case (Error(_), Ok(rue))  => ok
      case (Error(_), Error(_)) => ok
    }
  })

  def recoverWith = prop((x: DB[Int], y: DB[Int]) =>
    (x.recoverWith { case _ => y}).run(connection) must_== (x ||| y).run(connection)
  )

  def recoverWithSpecific = {
    val r = Result.fail[Int]("test")
    val a = DB.result(r)
    a.recoverWith { case This(_) => DB.value(3) } must beValue(3)
    a.recoverWith { case That(_) => DB.value(3) } must beResult(r)
  }

  def bracket = pending

  def result = prop((v: Result[Int]) =>
    DB.result(v) must beResult { v })

  def fail = prop((message: String) =>
    DB.fail(message) must beResult { Result.fail(message) })

  def exception = prop((t: Throwable) =>
    DB.exception(t) must beResult { Result.exception(t) })

  def safeDB = prop((t: Throwable) =>
    DB.ask(_ => throw t) must beResult { Result.exception(t) })

  def safeValue = prop((t: Throwable) =>
    DB.value(throw t) must beResult { Result.exception(t) })

  def guardMeansTrue = {
    DB.guard(true, "").run(connection) must beLike {
      case Ok(_) => ok
    }
    DB.guard(false, "").run(connection) must beLike {
      case Error(_) => ok
    }
  }

  def preventMeansFalse = {
    DB.prevent(true, "").run(connection) must beLike {
      case Error(_) => ok
    }
    DB.prevent(false, "").run(connection) must beLike {
      case Ok(_) => ok
    }
  }

  def ask = {
    DB.ask { implicit session =>
      sql"""
        SELECT NAME FROM TEST.CUSTOMER
      """.map(rs => rs.string(1)).list.apply()
    }.run(connection) must_== Ok(List("BRUCE", "WAYNE"))
  }

  def queryFirst = {
    implicit val idExtractor: Extractor[Long]       = new Extractor(_.long(1))
    implicit val streetExtractor: Extractor[String] = new Extractor(_.string(1))

    DB.queryFirst[Long](
      sql"""SELECT CUSTOMER_ID FROM TEST.CUSTOMER ASC"""
    ).flatMap {id => 
      DB.query[String](sql"""SELECT STREET FROM TEST.ADDRESS WHERE CUSTOMER_ID = ${id.get}""")
    }.run(connection) must_== Ok(List("WAYNE MANOR", "WAYNE HOUSE"))
  }

  def querySingle = {
    implicit val extractor: Extractor[(Long, String, Int)] = new Extractor(rs => (rs.long(1), rs.string(2), rs.int(3)))
    
    DB.queryFirst[(Long, String, Int)](
      sql"""SELECT * FROM TEST.CUSTOMER WHERE NAME = 'BRUCE'"""
    ).run(connection) must_== Ok(Some((1, "BRUCE", 37)))
  }

  def query = {
    case class Customer(id: Long, name: String, age: Int)
    implicit val extractor: Extractor[Customer] = new Extractor(rs => Customer(rs.long(1), rs.string(2), rs.int(3)))
    
    DB.query[Customer](
      sql"""SELECT * FROM TEST.CUSTOMER"""
    ).run(connection) must_== Ok(List(Customer(1, "BRUCE", 37), Customer(2, "WAYNE", 37)))
  }

  def setupDb = {
    SDB(connection) autoCommit { implicit session =>
      sql"""DROP SCHEMA IF EXISTS TEST CASCADE""".execute.apply()
      sql"""CREATE SCHEMA TEST AUTHORIZATION DBA""".execute.apply()
      sql"""
        CREATE TABLE TEST.CUSTOMER (
          CUSTOMER_ID      BIGINT GENERATED ALWAYS AS IDENTITY(START WITH 1) PRIMARY KEY,
          NAME             VARCHAR(25) NOT NULL,
          AGE              INTEGER  NOT NULL
        )
      """.execute.apply()

      sql"""
        CREATE TABLE TEST.ADDRESS (
          ADDRESS_ID    BIGINT GENERATED ALWAYS AS IDENTITY(START WITH 1) PRIMARY KEY,
          STREET        VARCHAR(100) NOT NULL,
          CITY          VARCHAR(50)  NOT NULL,
          STATE         VARCHAR(10)  NOT NULL,
          CUSTOMER_ID   BIGINT       NOT NULL 
        )
      """.execute.apply()
      sql"""
        ALTER TABLE TEST.ADDRESS 
          ADD FOREIGN KEY (CUSTOMER_ID) REFERENCES TEST.CUSTOMER(CUSTOMER_ID)
      """.execute.apply()

      val id1 = sql"""
        INSERT INTO TEST.CUSTOMER(NAME, AGE)
          VALUES ('BRUCE', 37)
      """.updateAndReturnGeneratedKey.apply()
      val id2 = sql"""
        INSERT INTO TEST.CUSTOMER(NAME, AGE)
          VALUES ('WAYNE', 37)
      """.updateAndReturnGeneratedKey.apply()

      sql"""
        INSERT INTO TEST.ADDRESS(STREET, CITY, STATE, CUSTOMER_ID)
          VALUES ('WAYNE MANOR', 'GOTHAM', 'NSW', $id1)
      """.updateAndReturnGeneratedKey.apply()
      sql"""
        INSERT INTO TEST.ADDRESS(STREET, CITY, STATE, CUSTOMER_ID)
          VALUES ('WAYNE HOUSE', 'GOTHAM', 'NSW', $id1)
      """.updateAndReturnGeneratedKey.apply()
      sql"""
        INSERT INTO TEST.ADDRESS(STREET, CITY, STATE, CUSTOMER_ID)
          VALUES ('SOME HOUSE', 'INNER CITY', 'NSW', $id2)
      """.updateAndReturnGeneratedKey.apply()
    }
  }

  /** Note these are not general purpose, specific to testing laws. */
  implicit def DBIntArbitrary: Arbitrary[DB[Int]] =
    Arbitrary(Arbitrary.arbitrary[Result[Int]].map(DB.result))

  implicit def DBBooleanArbitrary: Arbitrary[DB[Boolean]] =
    Arbitrary(Arbitrary.arbitrary[Result[Boolean]].map(DB.result))

  implicit def DBArbitrary[A : Arbitrary]: Arbitrary[DB[A]] =
    Arbitrary(arbitrary[Result[A]].map(DB.result))

  implicit def DBEqual: Equal[DB[Int]] =
    Equal.equal[DB[Int]]((a, b) =>
      a.run(connection) must_== b.run(connection))

  def beResult[A](expected: Result[A]): Matcher[DB[A]] =
    (h: DB[A]) => h.run(connection) must_== expected

  def beResultLike[A](expected: Result[A] => SpecResult): Matcher[DB[A]] =
    (h: DB[A]) => expected(h.run(connection))

  def beValue[A](expected: A): Matcher[DB[A]] =
    beResult(Result.ok(expected))
}
