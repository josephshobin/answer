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

import scala.reflect.macros.whitebox.Context

import au.com.cba.omnia.answer.Extractor

/**
  * This macro creates an `Extractor` for the provided type.
  * 
  * usage :-
  * {{{
  *
  *   val extractor = ExtractorMacro.mkExtractor[(Option[String], Option[Long])]
  *
  * }}}
  */
object ExtractorMacro {
  /** Creates an extractor for a singleton type or product. */
  def mkExtractor[A]: Extractor[A] = macro impl[A]

  def impl[A : c.WeakTypeTag](c: Context): c.Expr[Extractor[A]] = {
    import c.universe._

    val targetType: Type = c.universe.weakTypeOf[A]

    /** Fail compilation with nice error message. */
    def abort(msg: String) =
      c.abort(c.enclosingPosition, s"Can't create Extractor for $targetType: $msg")

    /** Process an individual column. 
      * TODO: Check for the availability of ScalikeJdbc TypeBinder for typ and
      * fail the compilation if one can't be found.
      */
    def processColumn(typ: Type, position: Int): Tree = q"rs.get[$typ]($position)"

    val targetTypes = targetType.decls.sorted.toList collect {
      case sym: TermSymbol if sym.isVal && sym.isCaseAccessor => sym.typeSignatureIn(targetType)
    }

    val extractors =
      if (targetTypes.isEmpty) processColumn(targetType, 1)
      else {
        val parts = targetTypes.zipWithIndex.map { case (typ, i) => processColumn(typ, i + 1) }
        q"(..$parts)"
      }

    c.Expr[Extractor[A]](q"""
      import au.com.cba.omnia.answer.Extractor
      Extractor(rs => $extractors)
    """)
  }
}


