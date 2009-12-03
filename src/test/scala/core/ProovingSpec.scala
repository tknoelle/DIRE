package core

/**
 * User: nowi
 * Date: 28.10.2009
 * Time: 16:37:19
 */

import com.jteigen.scalatest.JUnit4Runner

import containers.{CNFClauseStore}
import core.{ProofFound}
import domain.fol.ast._
import domain.fol.parsers.SPASSIntermediateFormatParser
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec


@RunWith(classOf[JUnit4Runner])
abstract class ProovingSpec extends Spec with ShouldMatchers {

  // create a proover
  val resolutionProover: Proving

  describe("A object implementing the Prooving Trait") {
    //¬dog(x) ∨ animal(x)
    //dog(fido)
    //¬animal(y) ∨ die(y)
    //
    //Negate the goal
    //¬die(fido)


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


    val C1 = StandardClause(Negation(Predicate("American", x)), Negation(Predicate("Weapon", y)),
      Negation(Predicate("Sells", x, y, z)), Negation(Predicate("Hostile", z)),
      Predicate("Criminal", x))

    val C2 = StandardClause(
      Negation(Predicate("Missile", x)),
      Negation(Predicate("Owns", nono, x)),
      Predicate("Sells", west, x, nono)
      )

    val C3 = StandardClause(
      Negation(Predicate("Enemy", x, america)),
      Predicate("Hostile", x)
      )


    val C4 = StandardClause(
      Negation(Predicate("Missile", x)),
      Predicate("Weapon", x)
      )

    val C5 = StandardClause(
      Predicate("Owns", nono, m1)
      )
    val C6 = StandardClause(
      Predicate("Missile", m1)
      )
    val C7 = StandardClause(
      Predicate("American", west)
      )

    val C8 = StandardClause(
      Predicate("Enemy", nono, america)
      )

    val goalClause = StandardClause(
      Negation(Predicate("Criminal", west))
      )


    val goalClause2 = StandardClause(
      Negation(Predicate("American", west))
      )






    // the curiosity killed the cat domain
    val tuna = Constant("Tuna")
    val jack = Constant("Jack")
    val curiosity = Constant("Curiosity")
    val loves = (x: FOLNode, y: FOLNode) => Predicate("Loves", x, y)
    val kills = (x: FOLNode, y: FOLNode) => Predicate("Kills", x, y)
    val cat = (x: FOLNode) => Predicate("Cat", x)
    val animal = (x: FOLNode) => Predicate("Animal", x)
    val f = (x: FOLNode) => Predicate("F", x)
    val g = (x: FOLNode) => Predicate("G", x)


    val A1 = StandardClause(animal(f(x)), loves(g(x), x))
    val A2 = StandardClause(Negation(loves(x, f(x))), loves(g(x), x))
    val B = StandardClause(Negation(animal(y)), Negation(kills(x, y)), Negation(loves(z, x)))
    val C = StandardClause(Negation(animal(x)), loves(jack, x))
    val D = StandardClause(kills(jack, tuna), kills(curiosity, tuna))
    val E = StandardClause(cat(tuna))
    val F = StandardClause(Negation(cat(x)), animal(x))
    val goalClauseCuriosity = StandardClause(Negation(kills(curiosity, tuna)))















    it("should prove that west is a criminal") {



      // prove that west is a criminal
      resolutionProover.prove(CNFClauseStore(goalClause, C1, C2, C3, C4, C5, C6, C7, C8)) should equal(ProofFound()) // prove that west is a criminal




    }

    it("should prove that west is an american") {


      // prove that west is an american
      resolutionProover.prove(CNFClauseStore(goalClause2, C1, C2, C3, C4, C5, C6, C7, C8)) should equal(ProofFound())


    }


    it("should not refute that west is not american") {
      // create a proover
      resolutionProover.prove(CNFClauseStore(
        StandardClause(Predicate("American", west)), C1, C2, C3, C4, C5, C6, C7, C8)) should not equal (ProofFound())


    }

    it("should not refute that west is not criminal") {
      // create a proover
      resolutionProover.prove(CNFClauseStore(
        StandardClause(Predicate("Criminal", west)), C1, C2, C3, C4, C5, C6, C7, C8)) should not equal (ProofFound())


    }


    it("should prove that curiosity killed the cat") {
      // create a proover
      resolutionProover.prove(CNFClauseStore(A1, A2, B, C, D, E, F, goalClauseCuriosity)) should equal(ProofFound())


    }


    it("should reason with the amino problem from anne but only the clause list") {
      val text: String = """list_of_clauses(axioms, cnf). clause( -> NEWATOMIC28(U) TinyPolarAminoAcid(U),1). clause( -> NEWATOMIC3(U) Size(U),2). clause( -> NEWATOMIC24(U) Hydrophobicity(U),3). clause( Hydrophobic(U) -> Hydrophobicity(U),4). clause( Polarity(U) -> RefiningFeature(U),5). clause( Large(U) -> Size(U),6). clause( Hydrophilic(U) -> Hydrophobicity(U),7). clause( SideChainStructure(U) -> RefiningFeature(U),8). clause( Size(U) -> RefiningFeature(U),9). clause( Hydrophobicity(U) -> RefiningFeature(U),10). clause( Charge(U) -> RefiningFeature(U),11). clause( Small(U) -> Size(U),12). clause( Tiny(U) -> Size(U),13). clause( T(U) -> SpecificAminoAcid(U),14). clause( T(U) D(U) -> ,15). clause( T(U) Y(U) -> ,16). clause( N(U) -> Small(skf0_213(U)),17). clause( W(U) -> Large(skf0_212(U)),18). clause( NEWATOMIC0(U) -> Large(skf0_211(U)),19). clause( T(U) P(U) -> ,20). clause( T(U) G(U) -> ,21). clause( D(U) -> Hydrophilic(skf0_208(U)),22). clause( M(U) -> Large(skf0_207(U)),23). clause( L(U) -> Large(skf0_206(U)),24). clause( D(U) -> Small(skf0_202(U)),25). clause( Y(U) -> Hydrophobic(skf0_199(U)),26). clause( Q(U) -> Large(skf0_198(U)),27). clause( F(U) -> Hydrophobic(skf0_197(U)),28). clause( F(U) -> Large(skf0_196(U)),29). clause( R(U) -> Large(skf0_192(U)),30). clause( E(U) -> Hydrophilic(skf0_189(U)),31). clause( hasSize(U,U1)+ -> Size(U1),32). clause( Large(U) NEWATOMIC3(U) -> ,33). clause( Small(U) NEWATOMIC3(U) -> ,34). clause( Tiny(U) NEWATOMIC3(U) -> ,35). clause( V(U) -> Small(skf0_183(U)),36). clause( R(U) -> Hydrophilic(skf0_179(U)),37). clause( A(U) -> Hydrophobic(skf0_178(U)),38). clause( hasHydrophobicity(U,U1)+ -> Hydrophobicity(U1),39). clause( T(U) H(U) -> ,40). clause( H(U) -> Hydrophilic(skf0_175(U)),41). clause( C(U) -> Hydrophobic(skf0_173(U)),42). clause( T(U) F(U) -> ,43). clause( I(U) -> Large(skf0_170(U)),44). clause( A(U) -> Tiny(skf0_169(U)),45). clause( T(U) L(U) -> ,46). clause( NEWATOMIC14(U) -> Tiny(skf0_164(U)),47). clause( H(U) -> Large(skf0_163(U)),48). clause( T(U) C(U) -> ,49). clause( P(U) -> Hydrophobic(skf0_161(U)),50). clause( T(U) K(U) -> ,51). clause( S(U) -> Hydrophilic(skf0_159(U)),52). clause( Small(U) Tiny(U) -> ,53). clause( T(U) I(U) -> ,54). clause( T(U) -> Tiny(skf0_155(U)),55). clause( C(U) -> Small(skf0_154(U)),56). clause( G(U) -> Hydrophobic(skf0_150(U)),57). clause( T(U) NEWATOMIC18(U) -> ,58). clause( Y(U) -> Large(skf0_148(U)),59). clause( T(U) M(U) -> ,60). clause( S(U) -> Tiny(skf0_147(U)),61). clause( K(U) -> Hydrophilic(skf0_146(U)),62). clause( T(U) Q(U) -> ,63). clause( Large(U) Small(U) -> ,64). clause( Q(U) -> Hydrophilic(skf0_145(U)),65). clause( T(U) R(U) -> ,66). clause( Hydrophobic(U) NEWATOMIC24(U) -> ,67). clause( Hydrophilic(U) NEWATOMIC24(U) -> ,68). clause( T(U) V(U) -> ,69). clause( V(U) -> Hydrophobic(skf0_141(U)),70). clause( Hydrophobic(U) Hydrophilic(U) -> ,71). clause( T(U) N(U) -> ,72). clause( E(U) -> Small(skf0_140(U)),73). clause( I(U) -> Hydrophobic(skf0_135(U)),74). clause( N(U) -> Hydrophilic(skf0_134(U)),75). clause( P(U) -> Small(skf0_132(U)),76). clause( T(U) A(U) -> ,77). clause( T(U) W(U) -> ,78). clause( T(U) E(U) -> ,79). clause( T(U) S(U) -> ,80). clause( T(U) -> Hydrophilic(skf0_123(U)),81). clause( K(U) -> Large(skf0_121(U)),82). clause( M(U) -> Hydrophobic(skf0_117(U)),83). clause( G(U) -> Tiny(skf0_116(U)),84). clause( L(U) -> Hydrophobic(skf0_114(U)),85). clause( W(U) -> Hydrophobic(skf0_113(U)),86). clause( Large(U) Tiny(U) -> ,87). clause( N(U) -> hasSize(U,skf0_213(U)),88). clause( W(U) -> hasSize(U,skf0_212(U)),89). clause( NEWATOMIC0(U) -> hasSize(U,skf0_211(U)),90). clause( D(U) -> hasHydrophobicity(U,skf0_208(U)),91). clause( M(U) -> hasSize(U,skf0_207(U)),92). clause( L(U) -> hasSize(U,skf0_206(U)),93). clause( D(U) -> hasSize(U,skf0_202(U)),94). clause( Y(U) -> hasHydrophobicity(U,skf0_199(U)),95). clause( Q(U) -> hasSize(U,skf0_198(U)),96). clause( F(U) -> hasHydrophobicity(U,skf0_197(U)),97). clause( F(U) -> hasSize(U,skf0_196(U)),98). clause( R(U) -> hasSize(U,skf0_192(U)),99). clause( E(U) -> hasHydrophobicity(U,skf0_189(U)),100). clause( V(U) -> hasSize(U,skf0_183(U)),101). clause( R(U) -> hasHydrophobicity(U,skf0_179(U)),102). clause( A(U) -> hasHydrophobicity(U,skf0_178(U)),103). clause( H(U) -> hasHydrophobicity(U,skf0_175(U)),104). clause( C(U) -> hasHydrophobicity(U,skf0_173(U)),105). clause( I(U) -> hasSize(U,skf0_170(U)),106). clause( A(U) -> hasSize(U,skf0_169(U)),107). clause( NEWATOMIC14(U) -> hasSize(U,skf0_164(U)),108). clause( H(U) -> hasSize(U,skf0_163(U)),109). clause( P(U) -> hasHydrophobicity(U,skf0_161(U)),110). clause( S(U) -> hasHydrophobicity(U,skf0_159(U)),111). clause( T(U) -> hasSize(U,skf0_155(U)),112). clause( C(U) -> hasSize(U,skf0_154(U)),113). clause( G(U) -> hasHydrophobicity(U,skf0_150(U)),114). clause( Y(U) -> hasSize(U,skf0_148(U)),115). clause( S(U) -> hasSize(U,skf0_147(U)),116). clause( K(U) -> hasHydrophobicity(U,skf0_146(U)),117). clause( Q(U) -> hasHydrophobicity(U,skf0_145(U)),118). clause( V(U) -> hasHydrophobicity(U,skf0_141(U)),119). clause( E(U) -> hasSize(U,skf0_140(U)),120). clause( I(U) -> hasHydrophobicity(U,skf0_135(U)),121). clause( N(U) -> hasHydrophobicity(U,skf0_134(U)),122). clause( P(U) -> hasSize(U,skf0_132(U)),123). clause( hasHydrophobicity(U,U1)+ NEWATOMIC15(U) -> ,124). clause( Hydrophobicity(U) -> Hydrophobic(U) Hydrophilic(U),125). clause( T(U) -> hasHydrophobicity(U,skf0_123(U)),126). clause( K(U) -> hasSize(U,skf0_121(U)),127). clause( hasSize(U,U1)+ NEWATOMIC7(U) -> ,128). clause( M(U) -> hasHydrophobicity(U,skf0_117(U)),129). clause( G(U) -> hasSize(U,skf0_116(U)),130). clause( L(U) -> hasHydrophobicity(U,skf0_114(U)),131). clause( W(U) -> hasHydrophobicity(U,skf0_113(U)),132). clause( hasHydrophobicity(U,U1)+ Y(U) -> Hydrophobic(U1),133). clause( hasHydrophobicity(U,U1)+ L(U) -> Hydrophobic(U1),134). clause( hasSize(U,U1)+ S(U) -> Tiny(U1),135). clause( hasSize(U,U1)+ L(U) -> Large(U1),136). clause( hasSize(U,U1)+ R(U) -> Large(U1),137). clause( hasHydrophobicity(U,U1)+ G(U) -> Hydrophobic(U1),138). clause( hasHydrophobicity(U,U1)+ W(U) -> Hydrophobic(U1),139). clause( hasSize(U,U1)+ Q(U) -> Large(U1),140). clause( hasHydrophobicity(U,U1)+ E(U) -> Hydrophilic(U1),141). clause( hasSize(U,U1)+ T(U) -> Tiny(U1),142). clause( Size(U) -> Small(U) Large(U) Tiny(U),143). clause( hasHydrophobicity(U,U1)+ F(U) -> Hydrophobic(U1),144). clause( hasSize(U,U1)+ K(U) -> Large(U1),145). clause( hasHydrophobicity(U,U1)+ I(U) -> Hydrophobic(U1),146). clause( hasHydrophobicity(U,U1)+ T(U) -> Hydrophilic(U1),147). clause( hasHydrophobicity(U,U1)+ M(U) -> Hydrophobic(U1),148). clause( hasSize(U,U1)+ G(U) -> Tiny(U1),149). clause( hasSize(U,U1)+ E(U) -> Small(U1),150). clause( hasHydrophobicity(U,U1)+ H(U) -> Hydrophilic(U1),151). clause( hasHydrophobicity(U,U1)+ P(U) -> Hydrophobic(U1),152). clause( hasHydrophobicity(U,U1)+ Q(U) -> Hydrophilic(U1),153). clause( hasSize(U,U1)+ I(U) -> Large(U1),154). clause( hasHydrophobicity(U,U1)+ V(U) -> Hydrophobic(U1),155). clause( hasHydrophobicity(U,U1)+ D(U) -> Hydrophilic(U1),156). clause( hasSize(U,U1)+ D(U) -> Small(U1),157). clause( hasSize(U,U1)+ P(U) -> Small(U1),158). clause( hasSize(U,U1)+ F(U) -> Large(U1),159). clause( hasSize(U,U1)+ V(U) -> Small(U1),160). clause( hasSize(U,U1)+ C(U) -> Small(U1),161). clause( hasHydrophobicity(U,U1)+ N(U) -> Hydrophilic(U1),162). clause( hasHydrophobicity(U,U1)+ C(U) -> Hydrophobic(U1),163). clause( hasHydrophobicity(U,U1)+ S(U) -> Hydrophilic(U1),164). clause( hasSize(U,U1)+ A(U) -> Tiny(U1),165). clause( hasSize(U,U1)+ M(U) -> Large(U1),166). clause( hasHydrophobicity(U,U1)+ A(U) -> Hydrophobic(U1),167). clause( hasSize(U,U1)+ N(U) -> Small(U1),168). clause( hasSize(U,U1)+ Y(U) -> Large(U1),169). clause( hasSize(U,U1)+ W(U) -> Large(U1),170). clause( hasSize(U,U1)+ H(U) -> Large(U1),171). clause( hasHydrophobicity(U,U1)+ R(U) -> Hydrophilic(U1),172). clause( hasHydrophobicity(U,U1)+ K(U) -> Hydrophilic(U1),173). clause( Tiny(U) hasSize(U1,U)+ NEWATOMIC26(U1) -> ,174). clause( Large(U) hasSize(U1,U)+ NEWATOMIC9(U1) -> ,175). clause( AminoAcid(U) -> T(U) S(U) L(U) V(U) Q(U) H(U) G(U) N(U) M(U) W(U) K(U) I(U) C(U) Y(U) A(U) R(U) F(U) D(U) E(U) P(U),176). clause( -> AliphaticAminoAcid(U) NEWATOMIC21(U),177). clause( -> Charge(U) NEWATOMIC17(U),178). clause( -> NEWATOMIC31(U) AromaticAminoAcid(U),179). clause( -> NEWATOMIC29(U) AminoAcid(U),180). clause( -> AminoAcid(U) NEWATOMIC32(U),181). clause( -> LargeAliphaticAminoAcid(U) NEWATOMIC10(U),182). clause( -> NEWATOMIC16(U) AminoAcid(U),183). clause( -> AminoAcid(U) NEWATOMIC7(U),184). clause( -> NEWATOMIC18(U) AminoAcid(U),185). clause( -> AminoAcid(U) NEWATOMIC15(U),186). clause( K(U) -> SpecificAminoAcid(U),187). clause( Y(U) -> SpecificAminoAcid(U),188). clause( R(U) -> SpecificAminoAcid(U),189). clause( PositiveChargedAminoAcid(U) -> AminoAcid(U),190). clause( PositiveChargedAminoAcid(U) -> NEWATOMIC2(U),191). clause( LargeAliphaticAminoAcid(U) -> NEWATOMIC0(U),192). clause( LargeAliphaticAminoAcid(U) -> AminoAcid(U),193). clause( LargeAliphaticAminoAcid(U) -> NEWATOMIC1(U),194). clause( NegativeChargedAminoAcid(U) -> AminoAcid(U),195). clause( F(U) -> SpecificAminoAcid(U),196). clause( SpecificAminoAcid(U) -> AminoAcid(U),197). clause( AromaticAminoAcid(U) -> AminoAcid(U),198). clause( AromaticAminoAcid(U) -> NEWATOMIC11(U),199). clause( W(U) -> SpecificAminoAcid(U),200). clause( Neutral(U) -> Charge(U),201). clause( Negative(U) -> Charge(U),202). clause( TinyPolarAminoAcid(U) -> AminoAcid(U),203). clause( S(U) -> SpecificAminoAcid(U),204). clause( G(U) -> SpecificAminoAcid(U),205). clause( N(U) -> SpecificAminoAcid(U),206). clause( P(U) -> SpecificAminoAcid(U),207). clause( Positive(U) -> Charge(U),208). clause( D(U) -> SpecificAminoAcid(U),209). clause( V(U) -> SpecificAminoAcid(U),210). clause( A(U) -> SpecificAminoAcid(U),211). clause( Q(U) -> SpecificAminoAcid(U),212). clause( AliphaticAminoAcid(U) -> AminoAcid(U),213). clause( AliphaticAminoAcid(U) -> NEWATOMIC19(U),214). clause( I(U) -> SpecificAminoAcid(U),215). clause( H(U) -> SpecificAminoAcid(U),216). clause( E(U) -> SpecificAminoAcid(U),217). clause( C(U) -> SpecificAminoAcid(U),218). clause( M(U) -> SpecificAminoAcid(U),219). clause( L(U) -> SpecificAminoAcid(U),220). clause( W(U) Y(U) -> ,221). clause( L(U) F(U) -> ,222). clause( K(U) P(U) -> ,223). clause( W(U) C(U) -> ,224). clause( Positive(U) Neutral(U) -> ,225). clause( R(U) E(U) -> ,226). clause( S(U) H(U) -> ,227). clause( I(U) E(U) -> ,228). clause( I(U) H(U) -> ,229). clause( W(U) K(U) -> ,230). clause( K(U) N(U) -> ,231). clause( I(U) R(U) -> ,232). clause( K(U) C(U) -> ,233). clause( Q(U) M(U) -> ,234). clause( D(U) -> Negative(skf0_205(U)),235). clause( F(U) -> Neutral(skf0_203(U)),236). clause( W(U) E(U) -> ,237). clause( Q(U) G(U) -> ,238). clause( W(U) N(U) -> ,239). clause( S(U) -> Neutral(skf0_200(U)),240). clause( K(U) M(U) -> ,241). clause( Q(U) P(U) -> ,242). clause( C(U) M(U) -> ,243). clause( V(U) P(U) -> ,244). clause( L(U) H(U) -> ,245). clause( NEWATOMIC4(U) -> Negative(skf0_195(U)),246). clause( Y(U) V(U) -> ,247). clause( M(U) -> Neutral(skf0_194(U)),248). clause( I(U) P(U) -> ,249). clause( H(U) D(U) -> ,250). clause( W(U) F(U) -> ,251). clause( R(U) A(U) -> ,252). clause( V(U) Q(U) -> ,253). clause( Y(U) P(U) -> ,254). clause( P(U) -> Neutral(skf0_188(U)),255). clause( V(U) E(U) -> ,256). clause( Positive(U) Negative(U) -> ,257). clause( R(U) -> Positive(skf0_187(U)),258). clause( L(U) C(U) -> ,259). clause( N(U) -> Neutral(skf0_185(U)),260). clause( C(U) -> Neutral(skf0_184(U)),261). clause( F(U) M(U) -> ,262). clause( C(U) R(U) -> ,263). clause( I(U) -> Neutral(skf0_181(U)),264). clause( A(U) M(U) -> ,265). clause( H(U) E(U) -> ,266). clause( Y(U) C(U) -> ,267). clause( K(U) I(U) -> ,268). clause( W(U) A(U) -> ,269). clause( V(U) N(U) -> ,270). clause( K(U) Q(U) -> ,271). clause( F(U) P(U) -> ,272). clause( K(U) -> Positive(skf0_172(U)),273). clause( L(U) P(U) -> ,274). clause( Y(U) Q(U) -> ,275). clause( W(U) -> Neutral(skf0_171(U)),276). clause( K(U) H(U) -> ,277). clause( Q(U) F(U) -> ,278). clause( F(U) E(U) -> ,279). clause( Y(U) A(U) -> ,280). clause( E(U) -> Negative(skf0_168(U)),281). clause( H(U) P(U) -> ,282). clause( Neutral(U) NEWATOMIC17(U) -> ,283). clause( Negative(U) NEWATOMIC17(U) -> ,284). clause( Positive(U) NEWATOMIC17(U) -> ,285). clause( K(U) R(U) -> ,286). clause( L(U) E(U) -> ,287). clause( C(U) V(U) -> ,288). clause( W(U) R(U) -> ,289). clause( N(U) M(U) -> ,290). clause( Y(U) M(U) -> ,291). clause( R(U) V(U) -> ,292). clause( W(U) Q(U) -> ,293). clause( H(U) M(U) -> ,294). clause( A(U) Q(U) -> ,295). clause( Y(U) R(U) -> ,296). clause( I(U) Y(U) -> ,297). clause( W(U) P(U) -> ,298). clause( W(U) M(U) -> ,299). clause( L(U) M(U) -> ,300). clause( W(U) H(U) -> ,301). clause( A(U) V(U) -> ,302). clause( Y(U) F(U) -> ,303). clause( R(U) Q(U) -> ,304). clause( W(U) S(U) -> ,305). clause( Y(U) H(U) -> ,306). clause( I(U) A(U) -> ,307). clause( G(U) M(U) -> ,308). clause( I(U) V(U) -> ,309). clause( K(U) E(U) -> ,310). clause( L(U) D(U) -> ,311). clause( G(U) -> Neutral(skf0_156(U)),312). clause( K(U) D(U) -> ,313). clause( K(U) F(U) -> ,314). clause( R(U) F(U) -> ,315). clause( I(U) F(U) -> ,316). clause( D(U) M(U) -> ,317). clause( H(U) -> Positive(skf0_151(U)),318). clause( F(U) H(U) -> ,319). clause( F(U) NEWATOMIC18(U) -> ,320). clause( I(U) NEWATOMIC18(U) -> ,321). clause( Y(U) NEWATOMIC18(U) -> ,322). clause( W(U) NEWATOMIC18(U) -> ,323). clause( M(U) NEWATOMIC18(U) -> ,324). clause( R(U) NEWATOMIC18(U) -> ,325). clause( V(U) NEWATOMIC18(U) -> ,326). clause( H(U) NEWATOMIC18(U) -> ,327). clause( Q(U) NEWATOMIC18(U) -> ,328). clause( L(U) NEWATOMIC18(U) -> ,329). clause( K(U) NEWATOMIC18(U) -> ,330). clause( C(U) H(U) -> ,331). clause( Q(U) H(U) -> ,332). clause( K(U) A(U) -> ,333). clause( L(U) V(U) -> ,334). clause( K(U) V(U) -> ,335). clause( Y(U) E(U) -> ,336). clause( E(U) M(U) -> ,337). clause( V(U) M(U) -> ,338). clause( V(U) D(U) -> ,339). clause( L(U) Q(U) -> ,340). clause( A(U) -> Neutral(skf0_142(U)),341). clause( R(U) P(U) -> ,342). clause( Y(U) -> Neutral(skf0_138(U)),343). clause( V(U) -> Neutral(skf0_137(U)),344). clause( I(U) M(U) -> ,345). clause( I(U) C(U) -> ,346). clause( T(U) -> Neutral(skf0_131(U)),347). clause( W(U) L(U) -> ,348). clause( L(U) I(U) -> ,349). clause( I(U) Q(U) -> ,350). clause( Q(U) -> Neutral(skf0_129(U)),351). clause( L(U) G(U) -> ,352). clause( H(U) G(U) -> ,353). clause( K(U) S(U) -> ,354). clause( Negative(U) Neutral(U) -> ,355). clause( R(U) M(U) -> ,356). clause( S(U) M(U) -> ,357). clause( L(U) Y(U) -> ,358). clause( N(U) H(U) -> ,359). clause( W(U) I(U) -> ,360). clause( NEWATOMIC2(U) -> Positive(skf0_122(U)),361). clause( C(U) Q(U) -> ,362). clause( W(U) G(U) -> ,363). clause( K(U) Y(U) -> ,364). clause( V(U) H(U) -> ,365). clause( L(U) R(U) -> ,366). clause( Q(U) E(U) -> ,367). clause( K(U) G(U) -> ,368). clause( R(U) H(U) -> ,369). clause( W(U) V(U) -> ,370). clause( L(U) -> Neutral(skf0_118(U)),371). clause( M(U) P(U) -> ,372). clause( Q(U) N(U) -> ,373). clause( K(U) L(U) -> ,374). clause( V(U) F(U) -> ,375). clause( hasCharge(U,U1)+ -> Charge(U1),376). clause( L(U) A(U) -> ,377). clause( A(U) H(U) -> ,378). clause( W(U) D(U) -> ,379). clause( L(U) N(U) -> ,380). clause( V(U) G(U) -> ,381). clause( D(U) -> hasCharge(U,skf0_205(U)),382). clause( F(U) -> hasCharge(U,skf0_203(U)),383). clause( S(U) -> hasCharge(U,skf0_200(U)),384). clause( NEWATOMIC4(U) -> hasCharge(U,skf0_195(U)),385). clause( M(U) -> hasCharge(U,skf0_194(U)),386). clause( P(U) -> hasCharge(U,skf0_188(U)),387). clause( R(U) -> hasCharge(U,skf0_187(U)),388). clause( N(U) -> hasCharge(U,skf0_185(U)),389). clause( C(U) -> hasCharge(U,skf0_184(U)),390). clause( I(U) -> hasCharge(U,skf0_181(U)),391). clause( K(U) -> hasCharge(U,skf0_172(U)),392). clause( W(U) -> hasCharge(U,skf0_171(U)),393). clause( E(U) -> hasCharge(U,skf0_168(U)),394). clause( G(U) -> hasCharge(U,skf0_156(U)),395). clause( H(U) -> hasCharge(U,skf0_151(U)),396). clause( A(U) -> hasCharge(U,skf0_142(U)),397). clause( Y(U) -> hasCharge(U,skf0_138(U)),398). clause( V(U) -> hasCharge(U,skf0_137(U)),399). clause( T(U) -> hasCharge(U,skf0_131(U)),400). clause( Q(U) -> hasCharge(U,skf0_129(U)),401). clause( NEWATOMIC2(U) -> hasCharge(U,skf0_122(U)),402). clause( L(U) -> hasCharge(U,skf0_118(U)),403). clause( hasCharge(U,U1)+ NEWATOMIC16(U) -> ,404). clause( AminoAcid(U) NEWATOMIC31(U) -> NEWATOMIC30(U),405). clause( hasCharge(U,U1)+ C(U) -> Neutral(U1),406). clause( hasCharge(U,U1)+ R(U) -> Positive(U1),407). clause( hasCharge(U,U1)+ G(U) -> Neutral(U1),408). clause( hasCharge(U,U1)+ H(U) -> Positive(U1),409). clause( hasCharge(U,U1)+ I(U) -> Neutral(U1),410). clause( hasCharge(U,U1)+ Q(U) -> Neutral(U1),411). clause( Charge(U) -> Positive(U) Negative(U) Neutral(U),412). clause( hasCharge(U,U1)+ F(U) -> Neutral(U1),413). clause( hasCharge(U,U1)+ V(U) -> Neutral(U1),414). clause( hasCharge(U,U1)+ W(U) -> Neutral(U1),415). clause( hasCharge(U,U1)+ E(U) -> Negative(U1),416). clause( hasCharge(U,U1)+ A(U) -> Neutral(U1),417). clause( hasCharge(U,U1)+ D(U) -> Negative(U1),418). clause( hasCharge(U,U1)+ S(U) -> Neutral(U1),419). clause( hasCharge(U,U1)+ Y(U) -> Neutral(U1),420). clause( hasCharge(U,U1)+ P(U) -> Neutral(U1),421). clause( hasCharge(U,U1)+ T(U) -> Neutral(U1),422). clause( hasCharge(U,U1)+ K(U) -> Positive(U1),423). clause( hasCharge(U,U1)+ L(U) -> Neutral(U1),424). clause( hasCharge(U,U1)+ M(U) -> Neutral(U1),425). clause( hasCharge(U,U1)+ N(U) -> Neutral(U1),426). clause( Negative(U) hasCharge(U1,U)+ NEWATOMIC22(U1) -> ,427). clause( Positive(U) hasCharge(U1,U)+ NEWATOMIC5(U1) -> ,428). clause( AminoAcid(U) NEWATOMIC10(U) -> NEWATOMIC8(U) NEWATOMIC9(U),429). clause( AminoAcid(U) NEWATOMIC28(U) -> NEWATOMIC26(U) NEWATOMIC27(U),430). clause( -> PositiveChargedAminoAcid(U) NEWATOMIC6(U),431). clause( -> NEWATOMIC25(U) Polarity(U),432). clause( -> NegativeChargedAminoAcid(U) NEWATOMIC23(U),433). clause( -> NEWATOMIC12(U) SideChainStructure(U),434). clause( Polar(U) -> Polarity(U),435). clause( NegativeChargedAminoAcid(U) -> NEWATOMIC4(U),436). clause( Aliphatic(U) -> SideChainStructure(U),437). clause( TinyPolarAminoAcid(U) -> NEWATOMIC14(U),438). clause( TinyPolarAminoAcid(U) -> NEWATOMIC13(U),439). clause( Non_Polar(U) -> Polarity(U),440). clause( Aromatic(U) -> SideChainStructure(U),441). clause( G(U) -> Non_Polar(skf0_215(U)),442). clause( C(U) -> Polar(skf0_214(U)),443). clause( A(U) D(U) -> ,444). clause( S(U) C(U) -> ,445). clause( I(U) G(U) -> ,446). clause( N(U) G(U) -> ,447). clause( S(U) F(U) -> ,448). clause( Q(U) -> Polar(skf0_210(U)),449). clause( F(U) -> Non_Polar(skf0_209(U)),450). clause( A(U) P(U) -> ,451). clause( T(U) -> Polar(skf0_204(U)),452). clause( S(U) N(U) -> ,453). clause( I(U) N(U) -> ,454). clause( G(U) E(U) -> ,455). clause( Y(U) -> Aromatic(skf0_201(U)),456). clause( A(U) G(U) -> ,457). clause( Non_Polar(U) Polar(U) -> ,458). clause( A(U) E(U) -> ,459). clause( L(U) -> Non_Polar(skf0_193(U)),460). clause( A(U) -> Non_Polar(skf0_191(U)),461). clause( G(U) -> Aliphatic(skf0_190(U)),462). clause( D(U) P(U) -> ,463). clause( L(U) -> Aliphatic(skf0_186(U)),464). clause( I(U) D(U) -> ,465). clause( S(U) D(U) -> ,466). clause( H(U) -> Polar(skf0_182(U)),467). clause( W(U) -> Aromatic(skf0_180(U)),468). clause( F(U) G(U) -> ,469). clause( Y(U) -> Polar(skf0_177(U)),470). clause( R(U) -> Aliphatic(skf0_176(U)),471). clause( M(U) -> Non_Polar(skf0_174(U)),472). clause( S(U) A(U) -> ,473). clause( R(U) D(U) -> ,474). clause( S(U) I(U) -> ,475). clause( S(U) Y(U) -> ,476). clause( A(U) N(U) -> ,477). clause( K(U) -> Polar(skf0_167(U)),478). clause( E(U) P(U) -> ,479). clause( P(U) -> Non_Polar(skf0_166(U)),480). clause( V(U) -> Non_Polar(skf0_165(U)),481). clause( D(U) E(U) -> ,482). clause( W(U) -> Non_Polar(skf0_162(U)),483). clause( D(U) -> Aliphatic(skf0_160(U)),484). clause( Y(U) N(U) -> ,485). clause( H(U) -> Aromatic(skf0_158(U)),486). clause( C(U) -> Aliphatic(skf0_157(U)),487). clause( Aliphatic(U) NEWATOMIC12(U) -> ,488). clause( Aromatic(U) NEWATOMIC12(U) -> ,489). clause( S(U) Q(U) -> ,490). clause( Polar(U) NEWATOMIC25(U) -> ,491). clause( Non_Polar(U) NEWATOMIC25(U) -> ,492). clause( Y(U) D(U) -> ,493). clause( N(U) -> Polar(skf0_153(U)),494). clause( Y(U) G(U) -> ,495). clause( F(U) N(U) -> ,496). clause( E(U) -> Aliphatic(skf0_152(U)),497). clause( C(U) NEWATOMIC18(U) -> ,498). clause( A(U) NEWATOMIC18(U) -> ,499). clause( N(U) NEWATOMIC18(U) -> ,500). clause( S(U) NEWATOMIC18(U) -> ,501). clause( G(U) NEWATOMIC18(U) -> ,502). clause( P(U) NEWATOMIC18(U) -> ,503). clause( D(U) NEWATOMIC18(U) -> ,504). clause( E(U) NEWATOMIC18(U) -> ,505). clause( G(U) D(U) -> ,506). clause( Aliphatic(U) Aromatic(U) -> ,507). clause( NEWATOMIC11(U) -> Aromatic(skf0_149(U)),508). clause( C(U) F(U) -> ,509). clause( C(U) A(U) -> ,510). clause( S(U) R(U) -> ,511). clause( L(U) S(U) -> ,512). clause( R(U) -> Polar(skf0_144(U)),513). clause( N(U) E(U) -> ,514). clause( A(U) F(U) -> ,515). clause( P(U) -> Aliphatic(skf0_143(U)),516). clause( E(U) -> Polar(skf0_139(U)),517). clause( S(U) E(U) -> ,518). clause( F(U) -> Aromatic(skf0_136(U)),519). clause( NEWATOMIC1(U) -> Aliphatic(skf0_133(U)),520). clause( C(U) E(U) -> ,521). clause( G(U) P(U) -> ,522). clause( NEWATOMIC13(U) -> Polar(skf0_130(U)),523). clause( hasSideChainStructure(U,U1)+ -> SideChainStructure(U1),524). clause( I(U) -> Non_Polar(skf0_128(U)),525). clause( V(U) -> Aliphatic(skf0_127(U)),526). clause( C(U) G(U) -> ,527). clause( Q(U) D(U) -> ,528). clause( S(U) -> Polar(skf0_126(U)),529). clause( K(U) -> Aliphatic(skf0_125(U)),530). clause( M(U) -> Aliphatic(skf0_124(U)),531). clause( N(U) D(U) -> ,532). clause( S(U) G(U) -> ,533). clause( S(U) P(U) -> ,534). clause( hasPolarity(U,U1)+ -> Polarity(U1),535). clause( R(U) N(U) -> ,536). clause( N(U) -> Aliphatic(skf0_120(U)),537). clause( C(U) N(U) -> ,538). clause( I(U) -> Aliphatic(skf0_119(U)),539). clause( C(U) P(U) -> ,540). clause( R(U) G(U) -> ,541). clause( C(U) D(U) -> ,542). clause( S(U) V(U) -> ,543). clause( Q(U) -> Aliphatic(skf0_115(U)),544). clause( N(U) P(U) -> ,545). clause( D(U) -> Polar(skf0_112(U)),546). clause( S(U) -> Aliphatic(skf0_111(U)),547). clause( NEWATOMIC19(U) -> Aliphatic(skf0_110(U)),548). clause( F(U) D(U) -> ,549). clause( A(U) -> Aliphatic(skf0_109(U)),550). clause( T(U) -> Aliphatic(skf0_108(U)),551). clause( G(U) -> hasPolarity(U,skf0_215(U)),552). clause( C(U) -> hasPolarity(U,skf0_214(U)),553). clause( SideChainStructure(U) -> Aliphatic(U) Aromatic(U),554). clause( Q(U) -> hasPolarity(U,skf0_210(U)),555). clause( F(U) -> hasPolarity(U,skf0_209(U)),556). clause( T(U) -> hasPolarity(U,skf0_204(U)),557). clause( Y(U) -> hasSideChainStructure(U,skf0_201(U)),558). clause( L(U) -> hasPolarity(U,skf0_193(U)),559). clause( A(U) -> hasPolarity(U,skf0_191(U)),560). clause( G(U) -> hasSideChainStructure(U,skf0_190(U)),561). clause( L(U) -> hasSideChainStructure(U,skf0_186(U)),562). clause( H(U) -> hasPolarity(U,skf0_182(U)),563). clause( W(U) -> hasSideChainStructure(U,skf0_180(U)),564). clause( Y(U) -> hasPolarity(U,skf0_177(U)),565). clause( R(U) -> hasSideChainStructure(U,skf0_176(U)),566). clause( M(U) -> hasPolarity(U,skf0_174(U)),567). clause( K(U) -> hasPolarity(U,skf0_167(U)),568). clause( P(U) -> hasPolarity(U,skf0_166(U)),569). clause( V(U) -> hasPolarity(U,skf0_165(U)),570). clause( W(U) -> hasPolarity(U,skf0_162(U)),571). clause( D(U) -> hasSideChainStructure(U,skf0_160(U)),572). clause( H(U) -> hasSideChainStructure(U,skf0_158(U)),573). clause( C(U) -> hasSideChainStructure(U,skf0_157(U)),574). clause( N(U) -> hasPolarity(U,skf0_153(U)),575). clause( E(U) -> hasSideChainStructure(U,skf0_152(U)),576). clause( NEWATOMIC11(U) -> hasSideChainStructure(U,skf0_149(U)),577). clause( R(U) -> hasPolarity(U,skf0_144(U)),578). clause( Polarity(U) -> Non_Polar(U) Polar(U),579). clause( P(U) -> hasSideChainStructure(U,skf0_143(U)),580). clause( E(U) -> hasPolarity(U,skf0_139(U)),581). clause( hasPolarity(U,U1)+ NEWATOMIC32(U) -> ,582). clause( F(U) -> hasSideChainStructure(U,skf0_136(U)),583). clause( NEWATOMIC1(U) -> hasSideChainStructure(U,skf0_133(U)),584). clause( NEWATOMIC13(U) -> hasPolarity(U,skf0_130(U)),585). clause( I(U) -> hasPolarity(U,skf0_128(U)),586). clause( V(U) -> hasSideChainStructure(U,skf0_127(U)),587). clause( S(U) -> hasPolarity(U,skf0_126(U)),588). clause( K(U) -> hasSideChainStructure(U,skf0_125(U)),589). clause( hasSideChainStructure(U,U1)+ NEWATOMIC29(U) -> ,590). clause( M(U) -> hasSideChainStructure(U,skf0_124(U)),591). clause( N(U) -> hasSideChainStructure(U,skf0_120(U)),592). clause( I(U) -> hasSideChainStructure(U,skf0_119(U)),593). clause( Q(U) -> hasSideChainStructure(U,skf0_115(U)),594). clause( D(U) -> hasPolarity(U,skf0_112(U)),595). clause( S(U) -> hasSideChainStructure(U,skf0_111(U)),596). clause( NEWATOMIC19(U) -> hasSideChainStructure(U,skf0_110(U)),597). clause( A(U) -> hasSideChainStructure(U,skf0_109(U)),598). clause( T(U) -> hasSideChainStructure(U,skf0_108(U)),599). clause( AminoAcid(U) NEWATOMIC6(U) -> NEWATOMIC5(U),600). clause( AminoAcid(U) NEWATOMIC21(U) -> NEWATOMIC20(U),601). clause( AminoAcid(U) NEWATOMIC23(U) -> NEWATOMIC22(U),602). clause( hasSideChainStructure(U,U1)+ Q(U) -> Aliphatic(U1),603). clause( hasSideChainStructure(U,U1)+ R(U) -> Aliphatic(U1),604). clause( hasPolarity(U,U1)+ C(U) -> Polar(U1),605). clause( hasPolarity(U,U1)+ N(U) -> Polar(U1),606). clause( hasSideChainStructure(U,U1)+ V(U) -> Aliphatic(U1),607). clause( hasPolarity(U,U1)+ V(U) -> Non_Polar(U1),608). clause( hasSideChainStructure(U,U1)+ Y(U) -> Aromatic(U1),609). clause( hasSideChainStructure(U,U1)+ T(U) -> Aliphatic(U1),610). clause( hasPolarity(U,U1)+ M(U) -> Non_Polar(U1),611). clause( hasSideChainStructure(U,U1)+ F(U) -> Aromatic(U1),612). clause( hasPolarity(U,U1)+ T(U) -> Polar(U1),613). clause( hasSideChainStructure(U,U1)+ P(U) -> Aliphatic(U1),614). clause( hasSideChainStructure(U,U1)+ I(U) -> Aliphatic(U1),615). clause( hasSideChainStructure(U,U1)+ G(U) -> Aliphatic(U1),616). clause( hasPolarity(U,U1)+ A(U) -> Non_Polar(U1),617). clause( hasPolarity(U,U1)+ I(U) -> Non_Polar(U1),618). clause( hasSideChainStructure(U,U1)+ C(U) -> Aliphatic(U1),619). clause( hasPolarity(U,U1)+ W(U) -> Non_Polar(U1),620). clause( hasSideChainStructure(U,U1)+ M(U) -> Aliphatic(U1),621). clause( hasSideChainStructure(U,U1)+ K(U) -> Aliphatic(U1),622). clause( hasPolarity(U,U1)+ K(U) -> Polar(U1),623). clause( hasSideChainStructure(U,U1)+ A(U) -> Aliphatic(U1),624). clause( hasPolarity(U,U1)+ L(U) -> Non_Polar(U1),625). clause( hasSideChainStructure(U,U1)+ S(U) -> Aliphatic(U1),626). clause( hasPolarity(U,U1)+ R(U) -> Polar(U1),627). clause( hasSideChainStructure(U,U1)+ L(U) -> Aliphatic(U1),628). clause( hasSideChainStructure(U,U1)+ N(U) -> Aliphatic(U1),629). clause( hasPolarity(U,U1)+ Q(U) -> Polar(U1),630). clause( hasPolarity(U,U1)+ D(U) -> Polar(U1),631). clause( hasSideChainStructure(U,U1)+ H(U) -> Aromatic(U1),632). clause( hasSideChainStructure(U,U1)+ D(U) -> Aliphatic(U1),633). clause( hasPolarity(U,U1)+ Y(U) -> Polar(U1),634). clause( hasSideChainStructure(U,U1)+ W(U) -> Aromatic(U1),635). clause( hasPolarity(U,U1)+ G(U) -> Non_Polar(U1),636). clause( hasPolarity(U,U1)+ H(U) -> Polar(U1),637). clause( hasSideChainStructure(U,U1)+ E(U) -> Aliphatic(U1),638). clause( hasPolarity(U,U1)+ P(U) -> Non_Polar(U1),639). clause( hasPolarity(U,U1)+ S(U) -> Polar(U1),640). clause( hasPolarity(U,U1)+ F(U) -> Non_Polar(U1),641). clause( hasPolarity(U,U1)+ E(U) -> Polar(U1),642). clause( Aliphatic(U) hasSideChainStructure(U1,U)+ NEWATOMIC20(U1) -> ,643). clause( Aliphatic(U) hasSideChainStructure(U1,U)+ NEWATOMIC8(U1) -> ,644). clause( Aromatic(U) hasSideChainStructure(U1,U)+ NEWATOMIC30(U1) -> ,645). clause( Polar(U) hasPolarity(U1,U)+ NEWATOMIC27(U1) -> ,646). end_of_list. """
      // parse
      val clauses = SPASSIntermediateFormatParser.parseClauseStore(text)

      clauses match {
        case None => false
        case Some(clauseStore) => {
          // start the reasoner
          resolutionProover.prove(clauseStore) should equal(CompletionFound())
        }
      }


    }


  }
}
