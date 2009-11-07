package core.resolution

/**
 * User: nowi
 * Date: 04.11.2009
 * Time: 17:43:55
 */

import com.jteigen.scalatest.JUnit4Runner

import config.TheoremProvingConfig1
import containers.{CNFClauseStore}
import domain.fol.ast._
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
class ResolutionSpec extends Spec with ShouldMatchers {
  val log = net.lag.logging.Logger.get
  val resolver = new BinaryResolver(TheoremProvingConfig1)
  describe("A object implementing the Resolution Trait") {
    //    it("should factorize some clauses") {
    //      // init with the resolution example from the AIMA Book page 298
    //
    //
    //
    //      val x = Variable("x")
    //      val y = Variable("y")
    //      val z = Variable("z")
    //      val west = Constant("West")
    //      val nono = Constant("Nono")
    //      val m1 = Constant("M1")
    //      val america = Constant("America")
    //
    //
    //      val C1 = Clause(Negation(Predicate("American", x)), Predicate("Weapon", y),
    //        Negation(Predicate("Sells", x, y, z)), Negation(Predicate("Hostile", z)),
    //        Predicate("Criminal", x))
    //
    //      val C2 = Clause(
    //        Negation(Predicate("Missile", x)),
    //        Negation(Predicate("Owns", nono, x)),
    //        Predicate("Sells", west, x, nono)
    //        )
    //
    //      val C3 = Clause(
    //        Negation(Predicate("Enemy", x, america)),
    //        Predicate("Hostile", x)
    //        )
    //
    //
    //      val C4 = Clause(
    //        Negation(Predicate("Missile", x)),
    //        Predicate("Weapon", x)
    //        )
    //
    //      val C5 = Clause(
    //        Predicate("Owns", nono, m1)
    //        )
    //      val C6 = Clause(
    //        Predicate("Missile", m1)
    //        )
    //      val C7 = Clause(
    //        Predicate("American", west)
    //        )
    //
    //      val C8 = Clause(
    //        Predicate("Enemy", nono, america)
    //        )
    //
    //
    //
    //
    //      val clauseStore = CNFClauseStore(C1, C2, C3, C4, C5, C6, C7, C8)
    //
    //
    //      // create a proover
    //
    //      // factorize a store
    //      factorizer.factorize(clauseStore)
    //
    //
    //    }

    it("should resolve the 2 clauses from the http://www.enm.bris.ac.uk/ai/enjl/logic/sld034.html") {
      // init with the resolution example from the AIMA Book page 298


      val y = Variable("x")
      val x = Variable("y")
      val z = Variable("z")

      val C1 = Clause(Predicate("man", x), Predicate("man", y), Negation(Predicate("in_love", x, y)))
      // x gets rewritten to y  , we need logical equals methods
      val C2 = Clause(Predicate("male", z), Negation(Predicate("man", z)))


      // factorize
      val conclusions = resolver.resolve(C1, C2)

      log.info("Resolved %s , %s --> %s", C1, C2, conclusions)


    }

    it("should resolve the 2 clauses from http://mathworld.wolfram.com/ResolutionPrinciple.html") {
      // init with the resolution example from the AIMA Book page 298


      val y = Variable("y")
      val x = Variable("x")
      val a = Constant("a")

      val C1 = Clause(Predicate("P", x), Predicate("Q", x))
      // x gets rewritten to y  , we need logical equals methods
      val C2 = Clause(Negation(Predicate("P", a)), Predicate("R", y))


      // factorize
      val conclusions: Set[Clause] = resolver.resolve(C1, C2)
      log.info("Resolved %s , %s --> %s", C1, C2, conclusions)
      conclusions should contain(Clause(Predicate("Q", a), Predicate("R", y)))
      conclusions should have size (1)


    }


    it("it should resolve the steps from the aima book page 298") {
      // init with the resolution example from the AIMA Book page 298


      val x = Variable("x")
      val y = Variable("y")
      val z = Variable("z")
      val west = Constant("West")
      val nono = Constant("Nono")
      val m1 = Constant("M1")
      val america = Constant("America")
      val sells = (x: FOLNode, y: FOLNode, z: FOLNode) => Predicate("Sells", x, y, z)
      val weapon = (x: FOLNode) => Predicate("Weapon", x)
      val american = (x: FOLNode) => Predicate("American", x)
      val hostile = (x: FOLNode) => Predicate("Hostile", x)
      val missile = (x: FOLNode) => Predicate("Missile", x)
      val owns = (x: FOLNode, y: FOLNode) => Predicate("Owns", x, y)
      val enemy = (x: FOLNode, y: FOLNode) => Predicate("Enemy", x, y)


      val C1 = Clause(Negation(Predicate("American", x)), Negation(Predicate("Weapon", y)),
        Negation(Predicate("Sells", x, y, z)), Negation(Predicate("Hostile", z)),
        Predicate("Criminal", x))

      val C2 = Clause(
        Negation(Predicate("Missile", x)),
        Negation(Predicate("Owns", nono, x)),
        Predicate("Sells", west, x, nono)
        )

      val C3 = Clause(
        Negation(Predicate("Enemy", x, america)),
        Predicate("Hostile", x)
        )


      val C4 = Clause(
        Negation(Predicate("Missile", x)),
        Predicate("Weapon", x)
        )

      val C5 = Clause(
        Predicate("Owns", nono, m1)
        )
      val C6 = Clause(
        Predicate("Missile", m1)
        )
      val C7 = Clause(
        Predicate("American", west)
        )

      val C8 = Clause(
        Predicate("Enemy", nono, america)
        )

      val goalClause = Clause(
        Negation(Predicate("Criminal", west))
        )

      // 1.) resolve goal clause with the C1
      val R1 = resolver.resolve(C1, goalClause)
      log.info("R1 : %s", R1)
      R1 should have size (1)
      R1 should contain(Clause(Negation(sells(west, y, z)), Negation(weapon(y)), Negation(american(west)), Negation(hostile(z))))


      // 2.) resolve C7 with R1
      val R2: Set[Clause] = resolver.resolve(C7, R1.toList.head)
      log.info("R2 : %s", R2)
      R2 should have size (1)
      R2 should contain(Clause(Negation(sells(west, y, z)), Negation(weapon(y)), Negation(hostile(z))))


      // 3.) resolve C4 with R2
      val R3: Set[Clause] = resolver.resolve(C4, R2.toList.head)
      log.info("R3 : %s", R3)
      R3 should have size (1)
      R3 should contain(Clause(Negation(sells(west, y, z)), Negation(missile(y)), Negation(hostile(z))))


      // 4.) resolve C6 with R3
      val R4: Set[Clause] = resolver.resolve(C6, R3.toList.head)
      log.info("R4 : %s", R4)
      R4 should have size (1)
      R4 should contain(Clause(Negation(sells(west, m1, z)), Negation(hostile(z))))


      // 5.) resolve C2 with R4
      val R5: Set[Clause] = resolver.resolve(C2, R4.toList.head)
      log.info("R5 : %s", R5)
      R5 should have size (1)
      R5 should contain(Clause(Negation(missile(m1)), Negation(owns(nono, m1)), Negation(hostile(nono))))


      // 6.) resolve C6 with R5
      val R6: Set[Clause] = resolver.resolve(C6, R5.toList.head)
      log.info("R6 : %s", R6)
      R6 should have size (1)
      R6 should contain(Clause(Negation(owns(nono, m1)), Negation(hostile(nono))))



      // 7.) resolve C5 with R6
      val R7: Set[Clause] = resolver.resolve(C5, R6.toList.head)
      log.info("R7 : %s", R7)
      R7 should have size (1)
      R7 should contain(Clause(Negation(hostile(nono))))



      // 8.) resolve C3 with R7
      val R8: Set[Clause] = resolver.resolve(C3, R7.toList.head)
      log.info("R8 : %s", R8)
      R8 should have size (1)
      R8 should contain(Clause(Negation(enemy(nono, america))))



      // 9.) resolve C8 with R8
      val R9: Set[Clause] = resolver.resolve(C8, R8.toList.head)
      log.info("R9 : %s", R9)
      R9 should have size (0)


    }


  }
}