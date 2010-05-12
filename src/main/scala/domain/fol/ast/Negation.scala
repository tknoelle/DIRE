package domain.fol.ast

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 17:17:26
 */

case class Negation(x: Sentence) extends Sentence {




  // simplyfiy double negation
  val filler = x match {
    case Negation(f) => f
    case _ => x
  }

  val top = "-" + filler.top

  val args = List(filler)


  override def flatArgs: List[FOLNode] = {
    args.map({x: FOLNode => x.flatArgs}).flatten
  }


  override def map(f: (FOLNode => FOLNode)): FOLNode = {
    // check all possible fol types
    Negation(filler.map(f))
  }

  override def toString = "¬(%s)" format (filler)

  override def logicalEquals(obj: Any) = {
    obj match {
      case Negation(otherFiller) => this.filler logicalEquals otherFiller
      case _ => false
    }
  }
}