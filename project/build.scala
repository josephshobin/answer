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
  val thermometerVersion = "0.5.3-20150113044449-b47d6dd"
  val omnitoolVersion    = "1.6.0-20150310053432-47e0688"

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
       , addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0" cross CrossVersion.full)
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
             "org.scalikejdbc"  %% "scalikejdbc"      % "2.1.2" exclude("joda-time", "joda-time")
           , "org.scalikejdbc"  %% "scalikejdbc-test" % "2.1.2"         % "test"
           , "org.hsqldb"        % "hsqldb"           % "2.3.2"         % "test"
           , "au.com.cba.omnia" %% "omnitool-core"    % omnitoolVersion % "test" classifier "tests"
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
           "org.scala-lang"   % "scala-compiler"   % sv
         , "org.scala-lang"   % "scala-reflect"    % sv
         , "org.scalamacros" %% "quasiquotes"      % "2.0.0"
         , "org.scalikejdbc" %% "scalikejdbc-test" % "2.1.2"  % "test"
         , "com.twitter"      % "util-eval_2.10"   % "6.22.1" % "test"
         , "org.hsqldb"       % "hsqldb"           % "2.3.2"  % "test"
         ) ++ depend.testing()
         )
       , addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0" cross CrossVersion.full)
    )
  ).dependsOn(core)
}
