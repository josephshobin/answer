#answer

[![Build Status](https://travis-ci.org/CommBank/answer.svg?branch=master)](https://travis-ci.org/CommBank/answer)

answer is a functional wrapper for [ScalikeJDBC](http://scalikejdbc.org). The 
central concept in answer is the `DB` monad which operates on a scalikejdbc 
`DBSession` to yield a [`Result`](https://commbank.github.io/omnitool/latest/api/index.html#au.com.cba.omnia.omnitool.Result) 
and gets all its combinators via the [`ResultantMonad`](https://commbank.github.io/omnitool/latest/api/index.html#au.com.cba.omnia.omnitool.ResultantMonad). 

A `DB` can be ran quickly with a `DBConfig` thereby allowing users to get some 
rudimentary form of sql connection pooling. However, advanced users might want 
to manage their own sql connections and can run `DB` with just a sql connection. 
The `DB` automatically manages transactions as part of its execution ensuring 
that an [`Ok`](https://commbank.github.io/omnitool/latest/api/index.html#au.com.cba.omnia.omnitool.Ok) 
transpires to a database commit while everything else is rolled back.

A simple use of the API:

```scala
import scalikejdbc.{DB => _, _}

import au.com.cba.omnia.omnitool._

import au.com.cba.omnia.answer._

case class User(firstName: String, lastName: String, age: Int)

val dbConfig               = DBConfig("jdbc:hsqldb:mem:userDB", "sa", "welcome")
implicit val userExtractor = new Extractor(rs => User(rs.string(1), rs.string(2), rs.int(3)))

DB.query[User](sql"""SELECT FIRST_NAME, LAST_NAME, AGE FROM USER""").run(dbConfig)
```
###Usage
See https://commbank.github.io/answer

[Scaladoc](https://commbank.github.io/answer/latest/api/index.html)
