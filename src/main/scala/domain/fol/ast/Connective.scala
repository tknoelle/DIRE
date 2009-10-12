package domain.fol.ast


/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:23:46
 */


abstract class Connective extends Sentence {
  val left, right: Sentence

  override def flatArgs: List[FOLNode] = {
    (left.args, right.args) match {
      case (Some(args1), Some(args2)) => (args1.map({x: FOLNode => x.flatArgs}) ::: args2.map({x: FOLNode => x.flatArgs})).flatten
      case (_, Some(args2)) => args2.map({x: FOLNode => x.flatArgs}).flatten
      case (Some(args1), _) => args1.map({x: FOLNode => x.flatArgs}).flatten
    }
  }
}

case class AndConnective(left: Sentence, right: Sentence) extends Connective {
  val symbolicName = "and"
  val args = Some(List(left, right))

  override def toString = "%s ∧ %s" format (left, right)
}
case class OrConnective(left: Sentence, right: Sentence) extends Connective {
  val symbolicName = "or"
  val args = Some(List(left, right))

  override def toString = "%s ∨ %s" format (left, right)
}
case class ImplicationConnective(left: Sentence, right: Sentence) extends Connective {
  val symbolicName = "->"
  val args = Some(List(left, right))

  override def toString = "%s ⇒ %s" format (left, right)
}
case class EqualityConnective(left: Sentence, right: Sentence) extends Connective {
  val symbolicName = "="
  val args = Some(List(left, right))

  override def toString = "%s ⇔ %s" format (left, right)
}