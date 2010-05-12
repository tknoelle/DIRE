package core.reduction


import collection.mutable.ListBuffer
import collection.mutable.{HashSet => MHashSet}
import domain.fol.ast.{FOLNode, NegativeFOLLiteral, PositiveFOLLiteral, FOLClause}

// import implicit converstion for FOLClausees

/**
 * User: nowi
 * Date: 24.04.2010
 * Time: 19:33:20
 */

trait ClauseTautologyDetection {
  def apply(clauseBuffer: List[FOLNode]): Boolean
}

trait ClauseStoreTautologyDeletion {
  def apply(clauseBuffer: List[FOLClause]): List[FOLClause]
}


object ClauseTautologyDetector extends ClauseTautologyDetection {

  def apply(clauseBuffer: List[FOLNode]): Boolean = {

    val positiveLits = new MHashSet[FOLNode]()
    val negativeLits = new MHashSet[FOLNode]()

    for (literal <- clauseBuffer.toList) {
      literal match {
        case PositiveFOLLiteral(node) => positiveLits += node
        case NegativeFOLLiteral(node) => negativeLits += node
      }


    }

    positiveLits.intersect(negativeLits)

    if (positiveLits.isEmpty) {
      false
    } else
      true

  }

}

object ClauseStoreTautologyDeletor extends ClauseStoreTautologyDeletion {
  def apply(clauseStore: List[FOLClause]): List[FOLClause] = {
    clauseStore.filter(!ClauseTautologyDetector(_))
  }
}