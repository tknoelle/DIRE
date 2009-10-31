package domain.fol.parsers

/**
 * User: nowi
 * Date: 25.09.2009
 * Time: 17:56:34
 */

import ast._
import com.jteigen.scalatest.JUnit4Runner

import org.junit.runner.RunWith


import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
class FOLDomainSpec extends Spec with ShouldMatchers {
  describe("The FOLDomain Objects") {
    it("literals should be only instantiable with atomic sentences and throw class cast exception if not") {
      val richard = Constant("richard");
      val john = Constant("john");

      // create literals
      val richardAndJohn = AndConnective(richard, john);

      // should not qualify as literal

      assert(richardAndJohn match {
        case FOLLiteral(x) => false
        case _ => true
      })

      assert(true)

    }


    it("clausel should be constructable from literals") {
      val richard = Constant("richard");
      val john = Constant("john");

      val clause = Clause(Set(richard, john));

      println(clause)

      assert(true)

    }

    it("clause should be able to return its positive/negative literals") {
      val richard = Constant("richard");
      val john = Constant("john");

      // create literals

      val clause = Clause(Set(richard, john))

      val positives = clause.positiveLiterals;
      val negatives = clause.negativeLiterals;

      // should contain richard
      assert(positives contains richard);
      assert(!(positives contains john));

      assert(negatives contains john);
      assert(!(negatives contains richard));

      assert(true);

    }


    it("a function should be able to return its nested variables") {
      // Loves(SF1(v2),v2)
      // Loves(v3,SF0(v3))
      // or
      // P(v1,SF0(v1),SF0(v1))
      // P(v2,SF0(v2),v2     )
      // or
      // P(v1,   F(v2),F(v2),F(v2),v1,      F(F(v1)),F(F(F(v1))),v2)
      // P(F(v3),v4,   v5,   v6,   F(F(v5)),v4,      F(v3),      F(F(v5)))

      val v2 = Variable("v2")
      val SF1 = Function("SF1", List(v2));
      val Loves = Function("Loves", List(SF1, v2));


      // get Variables from loves
      val lovesvars = Loves.vars;
      Loves.printVars
      Loves.printFlatArgs

      assert(Loves.vars contains v2);
      assert(Loves.vars.size == 2);


    }


  }

}
