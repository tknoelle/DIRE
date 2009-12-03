package domain.fol.parsers

/**
 * User: nowi
 * Date: 28.11.2009
 * Time: 12:08:56
 */

import com.jteigen.scalatest.JUnit4Runner

import org.junit.runner.RunWith


import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnit4Runner])
class SPASSParserSpec extends Spec with ShouldMatchers {
  describe("A SPASSParser") {
    //    it("should parse example problem pelletier57 from spass spec 1.5") {
    //      val text: String = "beginproblem(Pelletier57). listofdescriptions. name({' Pelletiers Problem No. 57 '}). author({' Christoph Weidenbach '}). status(unsatisfiable). description({' Problem taken in revised form from the Pelletier Collection, Journal of Automated Reasoning, Vol. 2, No. 2, pages 191-216 '}). endoflist. listofsymbols. functions[(f, 2), (a, 0), (b, 0), (c, 0)]. predicates[(F, 2)]. endoflist. listofformulae(axioms). formula(F(f(a,b),f(b,c))). formula(F(f(b,c),f(a,c))). formula(forall([U,V,W],implies(and(F(U,V),F(V,W)),F(U,W)))). endoflist. listofformulae(conjectures). formula(F(f(a,b),f(a,c))). endoflist. endproblem."
    //      // parse
    //      assert(SPASSParser.parse(text))
    //
    //    }
    //
    //



    //    it("should parse example problem pelletier57 from spass spec 1.5") {
    //      val text: String = """begin_problem(Pelletier57).
    //
    //list_of_descriptions.
    //name({*Pelletier's Problem No. 57*}).
    //author({*Christoph Weidenbach*}).
    //status(unsatisfiable).
    //description({*Problem taken in revised form from the "Pelletier Collection", Journal of Automated
    //	Reasoning, Vol. 2, No. 2, pages 191-216*}).
    //end_of_list.
    //
    //list_of_symbols.
    //  functions[(f,2), (a,0), (b,0), (c,0)].
    //  predicates[(F,2)].
    //end_of_list.
    //
    //list_of_formulae(axioms).
    //
    //formula(F(f(a,b),f(b,c))).
    //formula(F(f(b,c),f(a,c))).
    //formula(forall([U,V,W],implies(and(F(U,V),F(V,W)),F(U,W)))).
    //end_of_list.
    //
    //list_of_formulae(conjectures).
    //
    //formula(F(f(a,b),f(a,c))).
    //
    //end_of_list.
    //
    //end_problem."""
    //      // parse
    //      assert(SPASSParser.parse(text))
    //
    //    }

    //    it("should parse sokrates example") {
    //      val text: String = """begin_problem(Sokrates1).
    //
    //list_of_descriptions.
    //name({*Sokrates*}).
    //author({*Christoph Weidenbach*}).
    //status(unsatisfiable).
    //description({* Sokrates is mortal and since all humans are mortal, he is mortal too. *}).
    //end_of_list.
    //
    //list_of_symbols.
    //  functions[(sokrates,0)].
    //  predicates[(Human,1),(Mortal,1)].
    //end_of_list.
    //
    //list_of_formulae(axioms).
    //
    //formula(Human(sokrates),1).
    //formula(forall([x],implies(Human(x),Mortal(x))),2).
    //
    //end_of_list.
    //
    //list_of_formulae(conjectures).
    //
    //formula(Mortal(sokrates),3).
    //
    //end_of_list.
    //
    //end_problem."""
    //      // parse
    //      assert(SPASSParser.parse(text))
    //
    //    }

    it("should parse the proof problem") {
      val text: String = """begin_problem(ProofDemo).
list_of_descriptions.
name({*test.dfg*}).
author({*SPASS*}).
status(unsatisfiable).
description({*File generated by SPASS containing a proof.*}).
end_of_list.
list_of_symbols.
functions[(skf1, 1)].
predicates[(P, 2)].
end_of_list.
list_of_clauses(conjectures, cnf).
clause(forall([U],or(P(U,skf1(U)))),1).
clause(forall([U],or(not(P(skf1(U),U)))),2).
clause(forall([V,U,W],or(equal(U,V),equal(V,W),equal(W,U))),3).
end_of_list.
end_problem."""
      // parse
      assert(SPASSParser.parse(text))

    }


  }

}