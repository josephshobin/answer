//   Copyright 2014-2018 Commonwealth Bank of Australia
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
  val omnitoolVersion    = "1.15.7-20181018075855-52b83f2"

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
        ++ depend.omnia("omnitool-core", omnitoolVersion, "test").map(_ classifier "tests")
        ++ hadoopCP.modules.find(_.name == "commons-logging") // for scalikejdbc, depend on our CDH commons-logging (without the rest of CDH)
        ++ depend.scalikejdbc() ++ depend.hsqldb().map(_ % "test")
    )
  )

  lazy val macros = Project(
    id = "macros"
  , base = file("macros")
  , settings =
       standardSettings
    ++ uniform.project("answer-macros", "au.com.cba.omnia.answer.macros")
  ).dependsOn(core % "compile->compile;test->test")
}
