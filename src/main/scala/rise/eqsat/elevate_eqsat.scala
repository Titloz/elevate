package rise.eqsat

import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.core.strategies.predicate._ 
import elevate.core.strategies.debug_
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import rise.elevate.Rise
import rise.elevate.rules.traversal._
import elevate.core.strategies.Traversable

object elevate_eqsat {

    @strategy def eqsat: Strategy[Rise] = e => Success(e)
}

