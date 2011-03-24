package domain.fol.parsers

import ast._
import core.containers.{ClauseStorage, CNFClauseStore}
import helpers.Logging
import java.io.File
import scala.util.parsing.combinator.syntactical._
import scala.collection.immutable.Map

/**
* User: nowi
* Date: 27.11.2009
* Time: 17:59:38
*/
object SPASSIntermediateFormatParser extends StandardTokenParsers with Logging {
  lexical.delimiters ++= List("(", ")", ").", "[", "]", ".", ". ", ",", ", ", ";", "{", "}", "->", "+")
  lexical.reserved += ("", "exists", "forall", "and", "or", "not", "implies", "implied", "equiv", "clause", "cnf",
          "dnf", "listofclauses", "true", "false", "axioms", "conjectures", "listofformulae",
          "endoflist", "predicate", "subsort", "sort", "freely", "generatedby", "listofdeclarations",
          "listofsymbols", "sorts", "predicates", "functions", "listofdescriptions", "satisfiable", "unsatisfiable",
          "unknown", "endproblem", "beginproblem", "formula", "name", "author", "version", "logic", "status", "description",
          "date", "->", "listofsettings", "setflag", "setprecedence", "setselection")


  def problem = "beginproblem" ~ "(" ~ ident ~ ")." ~ description ~ ". " ~ logicalpart ~ "endproblem" ~ "."

  def problemwithsettings = "beginproblem" ~ "(" ~ ident ~ ")." ~ description ~ ". " ~ logicalpart ~  settings2 ~ ". " ~ "endproblem" ~ "."


  def description = "listofdescriptions" ~ ". " ~ name ~ author ~ opt(version) ~ opt(logic) ~ status ~ desc ~ opt(date) ~ "endoflist"


  def name = "name" ~ "(" ~ "{" ~ stringLit ~ "}" ~ ")."

  def author = "author" ~ "(" ~ "{" ~ stringLit ~ "}" ~ ")."

  def version = "version" ~ "(" ~ "{" ~ stringLit ~ "}" ~ ")."

  def logic = "logic" ~ "(" ~ "{" ~ stringLit ~ "}" ~ ")."

  def status = "status" ~ "(" ~ logstate ~ ")."

  def desc = "description" ~ "(" ~ "{" ~ stringLit ~ "}" ~ ")."

  def date = "date" ~ "(" ~ "{" ~ stringLit ~ "}" ~ ")."


  def logstate = "satisfiable" | "unsatisfiable" | "unknown"


  def logicalpart: Parser[Any] = symbollist ~ rep(formulalist) ~ rep(clauselist) ~ rep(prooflist)

  def symbollist = "listofsymbols" ~ ". " ~ opt(functions ~ ". ") ~ opt(predicates ~ ". ") ~ "endoflist" ~ ". "

  // def declarationlist = "listofdeclarations." ~ rep(declaration) ~ "endoflist"
  def declarationlist = "listofdeclarations." ~ "endoflist"

  // def declaration = subsortdec1 | termdec1 | preddec1 | gendec1
  //
  // def gendec1 = "sort" ~ sortsym ~ opt("freely") ~ "generatedby" ~ funclist ~ "."
  //
  // def funclist = "[" ~ repsep(funsym, ",") ~ "]."
  //
  // def subsortdec1 = "subsort(" ~ sortsym ~ "," ~ sortsym ~ ")."
  //
  // def termdec1 = "forall(" ~ termlist ~ "," ~ term ~ ")."
  //
  // def preddec1 = "predicate(" ~ predsym ~ rep1sep(sortsym, ",") ~ ")."
  //
  // def sortsym = ident

  def predsym = ident

  def funsym = ident

  def formulalist = "listofformulae" ~ "(" ~ origintype ~ ")." ~ rep(formula) ~ "endoflist" ~ ". "


  def formula = "formula" ~ "(" ~ term ~ "," ~ label ~ ")." | "formula" ~ "(" ~ term ~ ")." | "formula" ~ "(" ~ ")."

  def origintype = "axioms" | "conjectures"

  def label = numericLit


  def term: Parser[Sentence] = quantTerm | negation | connective | orConnective | andConnective | predicate | variable

  def predicate: Parser[Term] = ident ~ "(" ~ repsep(term, ",") ~ ")" ~ opt("+") ^^ {
    case ident ~ "(" ~ terms ~ ")" ~ None if (ident.toString.charAt(0).isLowerCase) => Function(ident.toString, terms)
    case ident ~ "(" ~ terms ~ ")" ~ None if (ident.toString.charAt(0).isUpperCase) => Predicate(ident.toString, terms)
    case ident ~ "(" ~ terms ~ ")" ~ Some(plus) if (ident.toString.charAt(0).isLowerCase) => Function(ident.toString, terms)
    case ident ~ "(" ~ terms ~ ")" ~ Some(plus) if (ident.toString.charAt(0).isUpperCase) => Predicate(ident.toString, terms)
    case _ => error("Should not be here")
  }

  def variable: Parser[Variable] = ident ^^ {
    case v => Variable(v.toString)
  }

  def connective: Parser[Connective] = ("equal" | "equiv" | "implies" | "implied") ~ "(" ~ repsep(term, ",") ~ ")" ^^ {
    case "equal" ~ "(" ~ terms ~ ")" => EqualityConnective(terms(0), terms(1))
    case "equiv" ~ "(" ~ terms ~ ")" => EqualityConnective(terms(0), terms(1))
    case "implies" ~ "(" ~ terms ~ ")" => ImplicationConnective(terms(0), terms(1))
    case "implied" ~ "(" ~ terms ~ ")" => ImplicationConnective(terms(1), terms(0))
  }

  def negation = "not" ~ "(" ~ term ~ ")" ^^ {
    case "not" ~ "(" ~ t ~ ")" => Negation(t)
  }

  def andConnective: Parser[Connective] = "and" ~ "(" ~ repsep(term, ",") ~ ")" ^^ {
    case "and" ~ "(" ~ terms ~ ")" => AndConnective(terms)
  }

  def orConnective: Parser[Connective] = "or" ~ "(" ~ repsep(term, ",") ~ ")" ^^ {
    case "or" ~ "(" ~ terms ~ ")" => OrConnective(terms)
  }


  def quantTerm: Parser[Quantifier] = ("exists" | "forall") ~ "(" ~ variableList ~ "," ~ term ~ ")" ^^ {
    case "forall" ~ "(" ~ vars ~ "," ~ t ~ ")" => UniversalQuantifer(t, vars)
    case "exists" ~ "(" ~ vars ~ "," ~ t ~ ")" => ExistentialQuantifer(t, vars)
  }

  def variableList: Parser[List[Variable]] = "[" ~ repsep(variable, ",") ~ "]" ^^ {
    case "[" ~ vars ~ "]" => {
      log.debug("Created variable list : %s", vars)
      vars
    }
  }


  def clauselist: Parser[List[FOLClause]] = "listofclauses" ~ "(" ~ origintype ~ "," ~ clausetype ~ ")." ~ rep(clause) ~ "endoflist" ~ ". " ^^ {
    case "listofclauses" ~ "(" ~ co ~ "," ~ ct ~ ")." ~ clauses ~ "endoflist" ~ ". " => clauses
  }



  def clause: Parser[ALCDClause] = "clause" ~ "(" ~ (cnfclause | dnfclause) ~ "," ~ label ~ ")." ^^ {
    case "clause" ~ "(" ~ c ~ "," ~ l ~ ")." => ALCDClause(c.args: _*)
  }


  def sharedClauselist: Parser[List[FOLClause]] = "listofclauses" ~ "(" ~ origintype ~ "," ~ clausetype ~ ")." ~ rep(sharedClause) ~ "endoflist" ~ ". " ^^ {
    case "listofclauses" ~ "(" ~ co ~ "," ~ ct ~ ")." ~ clauses ~ "endoflist" ~ ". " => clauses
  }


  def sharedClause: Parser[ALCDClause] = "clause" ~ "(" ~ (cnfclause | dnfclause) ~ "," ~ label ~ ")." ^^ {
    case "clause" ~ "(" ~ c ~ "," ~ l ~ ")." => ALCDClause(c.args.map(_.shared) : _*)
  }

  def clausetype = "cnf" | "dnf"



  //clause( eins || zwei -> drei ).
  //'||' kannst du ignorieren, das ist um die Sorts (erste Liste) vom Rest zu trennen. 'drei' sind positive literale die anderen sind negativ, das heisst bei den Literalen vor dem Pfeil muß ein 'not' davor wenn man es ins 'or' schreibt.
  //also:
  //clause(or(NEWATOMIC3(U), Size(U))).
  //clause(or(not(N(U)),not(P(U)))).






  def cnfclause: Parser[Sentence] = universalCNFClause


  def universalCNFClause: Parser[OrConnective] = opt(rep(predicate)) ~ "->" ~ opt(rep(predicate)) ^^ {
    case Some(negativeLiterals) ~ "->" ~ Some(positiveLiterals) => {
      val x = OrConnective(negativeLiterals.map({Negation(_)})) ++ OrConnective(positiveLiterals)
      log.debug("Crated universal cnf clause %s", x)
      x

    }

    case None ~ "->" ~ Some(positiveLiterals) => {
      val x = OrConnective(positiveLiterals)
      log.debug("Crated universal cnf clause %s", x)
      x

    }

    case Some(negativeLiterals) ~ "->" ~ None => {
      val x = OrConnective(negativeLiterals.map({Negation(_)}))
      log.debug("Crated universal cnf clause %s", x)
      x

    }

  }


  def negativeLiterals = repsep(term, " ") ^^ {
    case terms => {
      terms.map({Negation(_)})
    }

  }

  def positiveLiterals = repsep(term, " ") ^^ {
    case terms => {
      terms
    }

  }


  def dnfclause: Parser[Sentence] = existentialDNFClause | andConnective

  def existentialDNFClause: Parser[ExistentialQuantifer] = "exists" ~ "(" ~ variableList ~ "," ~ andConnective ~ ")" ^^ {
    case "exists" ~ "(" ~ vars ~ "," ~ connective ~ ")" => ExistentialQuantifer(connective, vars)
  }


  def arity = numericLit

  def prooflist = "listofproof" ~ "endoflist."

  def settings = "listofsettings(SPASS). {'" ~ "'} endoflist"

  def settings2 = "listofsettings" ~ flags ~ "setprecedence" ~ "(" ~ precedence ~ ")." ~ "setselection" ~ "(" ~ selections ~ ")." ~ "endoflist" ^^{
    case "listofsettings" ~ flags ~ "setprecedence" ~ "(" ~ precedence ~ ")." ~ "setselection" ~ "(" ~ selections ~ ")." ~ "endoflist" => precedence
  }

  def flags = rep("setflag" ~ "(" ~ flag ~ "," ~ arity ~ ").")

  def precedence = repsep(precedenceitem, ",")

  def selections = repsep(selection, ",")

  def flag = ident

  def precedenceitem = ident

  def selection = ident

  //def settings2 = "listofsettings" ~  "endoflist" ^^ {
   // case "listofsettings" ~ settings ~ "endoflist" => settings
  //}


  def functions = "functions" ~ "[" ~ funs ~ "]" ^^ {
    case "functions" ~ "[" ~ funs ~ "]" => funs
  }

  //def funs: Parser[List[Any]] = repsep(functiondef,  ",") ^^ {
   // case functiondef => List(functiondef)
  //}   //can cause a stackoverflow

  def funs: Parser[List[Any]] = rep1(functiondef,  "," ~ functiondef) ^^ {
    case functiondef => List(functiondef)
  }


  def functiondef = "(" ~ funsym ~ "," ~ arity ~ ")" ^^ {
    case "(" ~ funsym ~ "," ~ arity ~ ")" => Set(funsym, arity)
  }

  //def predicates = "predicates" ~ "[" ~ repsep(predicatedef, ",") ~ "]" ^^ {
  //  case predicatedef => List(predicatedef)
  //}  //can cause a stackoverflow

  def predicates = "predicates" ~ "[" ~ rep1(predicatedef, "," ~ predicatedef) ~ "]" ^^ {
    case predicatedef => List(predicatedef)
  }

  def predicatedef = predsym | "(" ~ predsym ~ "," ~ arity ~ ")" ^^ {
    case "(" ~ predsym ~ "," ~ arity ~ ")" => Set(predsym, arity)
    case predsym => predsym
  }

  // def sorts = "sorts[" ~ repsep(sort, ",") ~ "]."
  //
  // def sort = sortsym


  def convertInput(input: String): String = {
    // "_" --> ""
    // remove "'"
    // "*" --> "'"
    // remove line breaks

    /*   Replacements for functions and variables that begin with numbers */
    /*var out = input.replaceAll("(?<=\\()()(?=\\d)", "XYZ")
    out = out.replaceAll("(?<=\\S,)()(?=\\d)", "XYZ")
    out = out.replace("_", "").replace("'", "").replace("*", "'").replace(", ", ",").replace("\n", " ").replace("\t", "").replace("||", "")
    out      */

    //old replacements
    //input.replace("_", "").replace("'", "").replace("*", "'").replace(", ", ",").replace("\n", " ").replace("\t", "").replace("||", "")

    input.replace("_", "").replace("'", "").replace("*", "'").replace(", ", ",").replace("\n", " ").replace("\t", "").replace("||", "").replace("listofsettings(SPASS). {'", "listofsettings").replace("'} endoflist", "endoflist")

  }


  def parse(dsl: String) =
    {
      val tokens = new lexical.Scanner(convertInput(dsl))
      phrase(problem)(tokens) match {
        case Success(tree, _) => {
          println(tree)
          true

        }
        case e: NoSuccess => {
          Console.err.println(e)
          false
        }
      }


    }

  def parseSymbols(dsl: String) = {
      val tokens = new lexical.Scanner(convertInput(dsl))
      phrase(symbollist)(tokens) match {
        case Success(tree, _) => {
          Some(tree)

        }
        case e: NoSuccess => {
          Console.err.println(e)
          None
        }
      }
  }



  def parseClauseStore(dsl: String): Option[List[FOLClause]] =
    {
      val tokens = new lexical.Scanner(convertInput(dsl))
      phrase(clauselist)(tokens) match {
        case Success(tree, _) => {
          Some(tree)

        }
        case e: NoSuccess => {
          Console.err.println(e)
          None
        }
      }


    }

  def parseClauseStoreShared(dsl: String): Option[List[FOLClause]] =
    {
      val tokens = new lexical.Scanner(convertInput(dsl))
      phrase(sharedClauselist)(tokens) match {
        case Success(tree, _) => {
          Some(tree)

        }
        case e: NoSuccess => {
          Console.err.println(e)
          None
        }
      }


    }

  def parseSettings(dsl: String) = {
      val tokens = new lexical.Scanner(convertInput(dsl))
      phrase(settings2)(tokens) match {
        case Success(tree, _) => {
          Some(tree)

        }
        case e: NoSuccess => {
          Console.err.println(e)
          None
        }
      }
  }


  def parseFromFile(file: File): List[FOLClause] = {
    val lines = scala.io.Source.fromFile(file).mkString
    val text: String = lines // parse
    val clauses = SPASSIntermediateFormatParser.parseClauseStore(text)

    clauses match {
      case None => throw new IllegalStateException("Could not load clauses from file")
      case Some(clauses) => {
        clauses
      }
    }

  }

  def parseSharedFromFile(file: File): List[FOLClause] = {
    val lines = scala.io.Source.fromFile(file).mkString
    val text: String = lines // parse
    val clauses = SPASSIntermediateFormatParser.parseClauseStoreShared(text)

    clauses match {
      case None => throw new IllegalStateException("Could not load clauses from file")
      case Some(clauses) => {
        clauses
      }
    }

  }

  def parseDFGClauseFromFile(file: File): List[FOLClause] = {
    var clauses = List[FOLClause]()
    val lines = scala.io.Source.fromFile(file).mkString
    val text: String = lines
    var t = text.split("end_of_list.")
    for (x: String <- t) {

      var tmp = x
      println(tmp)
      while (tmp.startsWith("\n")) {
        tmp = tmp.replaceFirst("\n", "")
      }

        val clauses = SPASSIntermediateFormatParser.parseClauseStore(tmp.concat("end_of_list. "))
        clauses match {
          case None => throw new IllegalStateException("Could not load clauses from file")
          case Some(clauses) => {
            return clauses
          }
        }
      }
    throw new IllegalStateException("Could not load clauses from file")
  }



  def parseDFGFromFile(file: File):Map[String, Object] = {
    var content = Map[String, Object]()
    val lines = scala.io.Source.fromFile(file).mkString
    val text: String = lines
    var t = text.split("end_of_list.")
    for (x: String <- t) {

      var tmp = x
      println(tmp)
      while (tmp.startsWith("\n")) {
        tmp = tmp.replaceFirst("\n", "")
      }
      if (tmp.startsWith("begin_problem")) {
        println(tmp.concat("end_of_list. "))
      }
      else if (tmp.startsWith("list_of_symbols.")) {
        val symbols = SPASSIntermediateFormatParser.parseSymbols(tmp.concat("end_of_list. "))

        symbols match {
          case None => throw new IllegalStateException("Could not load symbols from file")
          case Some(symbols) => {
            println(symbols)
          }
        }
      }
      else if (tmp.startsWith("list_of_clauses")) {

        val clauses = SPASSIntermediateFormatParser.parseClauseStore(tmp.concat("end_of_list. "))
        clauses match {
          case None => throw new IllegalStateException("Could not load clauses from file")
          case Some(clauses) => {
            content = content + ("clauses" -> clauses)
          }
        }
      }
      else if (tmp.startsWith("list_of_settings")) {
        val settings = SPASSIntermediateFormatParser.parseSettings(tmp.concat("end_of_list"))
        settings match {
          case None => throw new IllegalStateException("Could not load settings from file")
          case Some(settings) => {
            println(settings)
            content = content + ("precedence" -> settings)
          }
        }
      }
    }
    //var content = parseDFG(text)  //returns the whole dfg file as one tree
    content
  }

  def parseDFG(dfg: String) = {
    val tokens = new lexical.Scanner(convertInput(dfg))

    if(dfg.contains("list_of_settings")){
      phrase(problemwithsettings)(tokens) match {
        case Success(tree, _) => {
          Some(tree)

        }
        case e: NoSuccess => {
          Console.err.println(e)
          None
        }
      }
    }
    else{
      phrase(problem)(tokens) match {
        case Success(tree, _) => {
          Some(tree)

        }
        case e: NoSuccess => {
          Console.err.println(e)
          None
        }
      }
    }
  }

}