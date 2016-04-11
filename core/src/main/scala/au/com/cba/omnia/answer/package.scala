package au.com.cba.omnia

//import scalaz._, Scalaz._, Free.Trampoline

//import org.slf4j.{Logger, LoggerFactory}

import au.com.cba.omnia.omnitool.{Result, ResultantMonad, RelMonad}
//import au.com.cba.omnia.omnitool.TrampolineResult.TrampoResult

// Instaniate DBT  //was: forming the monad: `DBSession => Trampoline[Result[_]]`
package object answer extends DBT[Result]()(RelMonad.SelfR[Result]) //(LoggerFactory.getLogger("au.com.cba.omnia.answer"))
