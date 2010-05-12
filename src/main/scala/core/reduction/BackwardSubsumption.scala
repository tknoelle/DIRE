package core.reduction

/**
 * User: nowi
 * Date: 08.05.2010
 * Time: 21:45:48
 */

import containers._
import domain.fol.ast.{ALCDClause, FOLNode, FOLClause}
import domain.fol.Substitution
import helpers.Logging

/**
 * User: nowi
 * Date: 24.04.2010
 * Time: 15:31:29
 */

trait BackwardSubsumption {
  def apply(clauseBuffer: List[FOLNode], clauseStorage: ClauseStorage)(implicit subsumptionCheck: Subsumption) : Iterable[FOLClause]
}




object BackwardSubsumer extends BackwardSubsumption with Logging {
  implicit def listofFOLNode2FOLClause(literals: List[FOLNode]): FOLClause = ALCDClause(literals)

  private def subsumed(clause: FOLClause, allClauses: ClauseStorage)(implicit subsumptionCheck: Subsumption) = {
    // get all clausees , check if we have a index
    // we need only to query on one literal from the clause because
    // a subsumer clause will need to be more general in all literals anyway so we cannot miss
    // this is a case of imperfect filtering
    val queryLiteral = clause.literals.head

    val candidateClauses = allClauses match {
    // retrieve based on the index type .. TODO make this check outside this tight loop
    // more in the configuratoin or initialization of the robinson proover !
      case indexedClauseStore: BackwardMatchingInstanceClauseRetrieval => {
        indexedClauseStore.retrieveBackwardMatchingInstances(queryLiteral)
      }

      case indexedClauseStore: InstanceClauseRetrieval => {
        // next best index is a matching instantiations supporting index
        indexedClauseStore.retrieveInstances(queryLiteral)
      }

      case _ => {
        // no index support
        log.warning("Performing backward matching without index support !")
        // copy over all clauses from the claus store -- expensive
        allClauses.toList
      }
    }

    // from those candidates try to find one canddate that subsumes the clause
    candidateClauses.filter(subsumptionCheck(clause, _)) match {
      case Nil => {
        // nothin backwardsubsumned
        Nil
      }
      case subsumedClauses => {
        log.debug("%s detected that clause : %s backwardsubsumes clauses %s", this, clause, subsumedClauses)
        subsumedClauses
      }
    }
  }


  override def apply(clauseBuffer: List[FOLNode], clauseStore: ClauseStorage)(implicit subsumptionCheck: Subsumption) = {
    subsumed(clauseBuffer, clauseStore)
  }

}