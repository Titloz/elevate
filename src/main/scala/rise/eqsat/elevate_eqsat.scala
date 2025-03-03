package rise.eqsat

import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.core.strategies.predicate._ 
import elevate.core.strategies.debug_
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import rise.elevate.Rise
import rise.elevate.rules.traversal._
import elevate.core.strategies.Traversable
 import elevate.macros.StrategyMacro
 
object elevate_eqsat {

    // we define two different equivalence provers here : they are interesting in the context of guided eqsat where we give 
    // complete terms as guides
    // would be nice to have sketches instead here 

    // unsure I should use the macro expansion here
    @strategy def prove_equiv_BENF(rules: Seq[Rewrites], normRules: Seq[RewriteDirected] = BENF.directedRules): Rise => Strategy[Rise] =
        t => p => try {
            ProveEquiv.init().runBENF(t, p, rules, normRules)
        } catch {
            case e: CouldNotProveEquiv => Failure(prove_equiv_BENF(rules,normRules))
        } finally {
            return Success(p)
        }
    
    // unsure I should use the macro expansion here
    @strategy def prove_equiv_CNF(rules: Seq[Rewrites], normRules: Seq[RewriteDirected] = BENF.directedRules): Rise => Strategy[Rise] =
        t => p => try {
            ProveEquiv.init().runCNF(t, p, rules, normRules)
        } catch {
            case e: CouldNotProveEquiv => Failure(prove_equiv_CNF(rules, normRules))
        } finally {
            return Success(p)
        }

        // idem + informal : it can't work because I am not using any new guides at all
        // I need to declare some function f : Int => D with D the nice domain such that
        // on input n, f creates a new function g taking as an input a vector x of n terms 
        // and apply the prove_equiv n times with x[i] and x[i+1] as terms
        // if it fails at some point, then fail
    // @strategy def N_prove_equiv_CNF(rules: Seq[Rewrites], normRules: Seq[RewriteDirected] = BENF.directedRules, n: int): Rise => Strategy[Rise] = 
     //   t => p => if (n>0) then {prove_equiv_CNF(rules, normRules)(t,p) |> N_prove_equiv_CNF(rules, normRules, n-1)(p)} 
     //   else {id(p)}

    // it is not so satisfying in terms of the syntax. but i need some way to have guides ... 
    // we only deal with concrete terms here. need to implement the same thing for sketches also
    def guided_prove_equiv_CNF(rules: Seq[Rewrites], normRules: Seq[RewriteDirected] = CNF.directedRules, l: List[Rise]) : Rise = 
        l match {
            case Nil => Failure(guided_prove_equiv_CNF(rules, normRules, l));
            case head :: Nil => Failure(guided_prove_equiv_CNF(rules, normRules, l));
            case t :: p :: Nil => prove_equiv_CNF(rules, normRules)(t,p);
            case t :: p :: tail => prove_equiv_CNF(rules, normRules)(t,p) match {
                case Failure(x) => Failure(x);
                case Success(term) => guided_prove_equiv_CNF(rules, normRules, (p::tail));
            }
        }
    
    def guided_prove_equiv_BENF(rules: Seq[Rewrites], normRules: Seq[RewriteDirected] = BENF.directedRules, l: List[Rise]) : Rise = 
        l match {
            case Nil => Failure(guided_prove_equiv_BENF(rules, normRules, l));
            case head :: Nil => Failure(guided_prove_equiv_BENF(rules, normRules, l));
            case t :: p :: Nil => prove_equiv_BENF(rules, normRules)(t,p);
            case t :: p :: tail => prove_equiv_BENF(rules, normRules)(t,p) match {
                case Failure(x) => Failure(x);
                case Success(_) => guided_prove_equiv_BENF(rules, normRules, (p::tail));
            }
        }
    // @strategy def guided_prove_equiv  
    //we want something like Success(eqsat'(t)) where eqsat'(t) is just the best term found by eqsat
    @strategy def eqsat(iterations: int): Strategy[Rise] = t => Success(t) 

}

