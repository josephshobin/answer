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

import sbt._, Keys._

import au.com.cba.omnia.uniform.core.standard.StandardProjectPlugin._
import au.com.cba.omnia.uniform.core.version.UniqueVersionPlugin._
import au.com.cba.omnia.uniform.dependency.UniformDependencyPlugin._

object build extends Build {
  val thermometerVersion = "1.5.2-20170124200333-758c16c"
  val omnitoolVersion    = "1.14.4-20161223050214-5770b8e"
  val scalikejdbcVersion = "2.4.0"
  val hsqldbVersion      = "2.3.4"

  lazy val standardSettings =
    Defaults.coreDefaultSettings ++
    uniformDependencySettings ++
    uniform.docSettings("https://github.com/CommBank/answer") ++
    Seq(updateOptions := updateOptions.value.withCachedResolution(true))

  lazy val all = Project(
    id = "all"
  , base = file(".")
  , settings =
       standardSettings
    ++ uniform.project("answer-all", "au.com.cba.omnia.answer")
    ++ uniform.ghsettings
    ++ Seq(
         publishArtifact := false
       , addCompilerPlugin(depend.macroParadise())
    )
  , aggregate = Seq(core, macros)
  )

  lazy val core = Project(
    id = "core"
  , base = file("core")
  , settings =
       standardSettings
    ++ uniform.project("answer-core", "au.com.cba.omnia.answer.core")
    ++ Seq(
      libraryDependencies ++=
           depend.scalaz()
        ++ depend.testing() ++ depend.time()
        ++ depend.omnia("omnitool-core", omnitoolVersion)
        ++ Seq(
             "org.scalikejdbc"  %% "scalikejdbc"          % scalikejdbcVersion exclude("joda-time", "joda-time")
           , "org.scalikejdbc"  %% "scalikejdbc-test"     % scalikejdbcVersion    % "test"
           , "org.hsqldb"        % "hsqldb"               % hsqldbVersion         % "test"
           , "au.com.cba.omnia" %% "omnitool-core"        % omnitoolVersion       % "test" classifier "tests"
           , "org.specs2"       %% "specs2-matcher-extra" % depend.versions.specs % "test"
        )
    )
  )

  lazy val macros = Project(
    id = "macros"
  , base = file("macros")
  , settings =
       standardSettings
    ++ uniform.project("answer-macros", "au.com.cba.omnia.answer.macros")
    ++ Seq(
         libraryDependencies <++= scalaVersion.apply(sv => Seq(
           "org.scala-lang"   % "scala-compiler"       % sv
         , "org.scala-lang"   % "scala-reflect"        % sv
         , "org.scalikejdbc" %% "scalikejdbc-test"     % scalikejdbcVersion    % "test"
         , "com.twitter"     %% "util-eval"            % "6.24.0"              % "test"
         , "org.hsqldb"       % "hsqldb"               % hsqldbVersion         % "test"
         , "org.specs2"      %% "specs2-matcher-extra" % depend.versions.specs % "test"
         ) ++ depend.testing()
         )
       , addCompilerPlugin(depend.macroParadise())
    )
  ).dependsOn(core)
}
