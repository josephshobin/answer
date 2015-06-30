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

import org.specs2.{Specification, ScalaCheck}
import org.specs2.matcher.{TerminationMatchers, ThrownExpectations, Matcher}
import org.specs2.execute.{Result => SpecResult}

import org.scalacheck.Arbitrary, Arbitrary.arbitrary

import au.com.cba.omnia.omnitool.{Result, Ok, Error}
import au.com.cba.omnia.omnitool.test.OmnitoolProperties.resultantMonad
import au.com.cba.omnia.omnitool.test.Arbitraries._

class DBSpec extends Specification
  with TerminationMatchers
  with ThrownExpectations
  with ScalaCheck { def is = s2"""

DB Operations
=============

DB operations should:
  obey monad laws                                         ${resultantMonad.laws[DB]}

DB query:
  can ask for data                                        $ask
  can do a queryFirst                                     $queryFirst
  can do a querySingle                                    $querySingle
  will error when querySingle returns more than one row   $querySingleWithError
  can do a query for list                                 $query

"""
  val conf = DBConfig("jdbc:hsqldb:mem:test", "sa", "")
  setupDb

  def connection: Connection =  {
    DB.connection(conf).toOption.get
  }

  def ask = {
    DB.ask { implicit session =>
      sql"""
        SELECT NAME FROM TEST.CUSTOMER
      """.map(rs => rs.string(1)).list.apply()
    }.run(conf) must_== Ok(List("BRUCE", "WAYNE"))
  }

  def queryFirst = {
    implicit val idExtractor: Extractor[Long]       = new Extractor(_.long(1))
    implicit val streetExtractor: Extractor[String] = new Extractor(_.string(1))

    DB.queryFirst[Long](sql"""SELECT CUSTOMER_ID FROM TEST.CUSTOMER ASC""").flatMap(id =>
      DB.query[String](sql"""SELECT STREET FROM TEST.ADDRESS WHERE CUSTOMER_ID = ${id.get}""")
    ).run(conf) must_== Ok(List("WAYNE MANOR", "WAYNE HOUSE"))
  }

  // def queryFirstFor = {
  //   implicit val idExtractor: Extractor[Long]       = new Extractor(_.long(1))
  //   implicit val streetExtractor: Extractor[String] = new Extractor(_.string(1))

  //   (for {
  //     id      <- DB.queryFirst[Long](sql"""SELECT CUSTOMER_ID FROM TEST.CUSTOMER ASC""")
  //     streets <- DB.query[String](sql"""SELECT STREET FROM TEST.ADDRESS WHERE CUSTOMER_ID = ${id.get}""")
  //   } yield streets)
  //     .run(conf) must_== Ok(List("WAYNE MANOR", "WAYNE HOUSE"))
  // }

  // def queryFirstMinParens = {
  //   implicit val idExtractor: Extractor[Long]       = new Extractor(_.long(1))
  //   implicit val streetExtractor: Extractor[String] = new Extractor(_.string(1))

  //   { DB.queryFirst[Long](sql"""SELECT CUSTOMER_ID FROM TEST.CUSTOMER ASC""") flatMap[Traversable[String], Id] (id =>
  //     DB.query[String](sql"""SELECT STREET FROM TEST.ADDRESS WHERE CUSTOMER_ID = ${id.get}""")
  //   )}.run(conf) must_== Ok(List("WAYNE MANOR", "WAYNE HOUSE"))
  // }

  // Requires a generalized flatMap[B, M: RelMonadDB] defined via rBind
  // def queryFirstRelFor = {
  //   implicit val idExtractor: Extractor[Long]       = new Extractor(_.long(1))
  //   implicit val streetExtractor: Extractor[String] = new Extractor(_.string(1))

  //   (for {
  //     (id: Id[Option[Long]])      <- DB.queryFirst[Long](sql"""SELECT CUSTOMER_ID FROM TEST.CUSTOMER ASC""")
  //     (streets: Id[Traversable[String]]) <- DB.query[String](sql"""SELECT STREET FROM TEST.ADDRESS WHERE CUSTOMER_ID = ${id.get}""")
  //   } yield streets)
  //     .run(conf) must_== Ok(List("WAYNE MANOR", "WAYNE HOUSE"))
  // }

  def querySingle = {
    implicit val extractor: Extractor[(Long, String, Int)] = new Extractor(rs => (rs.long(1), rs.string(2), rs.int(3)))

    DB.querySingle[(Long, String, Int)](
      sql"""SELECT * FROM TEST.CUSTOMER WHERE NAME = 'BRUCE'"""
    ).run(conf) must_== Ok(Some((1, "BRUCE", 37)))
  }

  def querySingleWithError = {
    implicit val extractor: Extractor[(Long, String, Int)] = new Extractor(rs => (rs.long(1), rs.string(2), rs.int(3)))

    DB.querySingle[(Long, String, Int)](
      sql"""SELECT * FROM TEST.CUSTOMER"""
    ).run(conf) must beLike {
      case Error(_) => ok
    }
  }

  def query = {
    case class Customer(id: Long, name: String, age: Int)
    implicit val extractor: Extractor[Customer] = new Extractor(rs => Customer(rs.long(1), rs.string(2), rs.int(3)))

    DB.query[Customer](
      sql"""SELECT * FROM TEST.CUSTOMER"""
    ).run(conf) must_== Ok(List(Customer(1, "BRUCE", 37), Customer(2, "WAYNE", 37)))
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
    Arbitrary(Arbitrary.arbitrary[Result[Int]].map(DB.result(_)))

  implicit def DBBooleanArbitrary: Arbitrary[DB[Boolean]] =
    Arbitrary(Arbitrary.arbitrary[Result[Boolean]].map(DB.result(_)))

  implicit def DBArbitrary[A : Arbitrary]: Arbitrary[DB[A]] =
    Arbitrary(arbitrary[Result[A]].map(DB.result(_)))

  implicit def DBEqual: Equal[DB[Int]] =
    Equal.equal[DB[Int]]((a, b) =>
      a.run(conf) must_== b.run(conf))

  def beResult[A](expected: Result[A]): Matcher[DB[A]] =
    (h: DB[A]) => h.run(conf) must_== expected

  def beResultLike[A](expected: Result[A] => SpecResult): Matcher[DB[A]] =
    (h: DB[A]) => expected(h.run(conf))

  def beValue[A](expected: A): Matcher[DB[A]] =
    beResult(Result.ok(expected))
}
