package domain.fol

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 16:53:52
 */


trait Sentence


object Sentence {
  implicit def sentenceToAndConnective(x: Sentence): AndConnective = x.asInstanceOf[AndConnective]

  implicit def sentenceToTerm(x: Sentence): Term = x.asInstanceOf[Term]
}


