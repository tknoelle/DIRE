package partitioning

import core.containers.{CNFClauseStore, ClauseStorage, Node}
import domain.fol.ast.{NegativeFOLLiteral, PositiveFOLLiteral, FOLNode, FOLClause}
import domain.fol.parsers.SPASSIntermediateFormatParser
import helpers.Logging
import java.io.File
import scala.util.matching.Regex




class Partition extends ClauseStoragePartitioning with Logging{

  val parser = SPASSIntermediateFormatParser

  override def partition(clauses: ClauseStorage) = {
    val module0 = SPASSIntermediateFormatParser.parseFromFile(new File("input/conf/aminoacid_clauses.dfg"))

    val t = getNodeWeight(module0)
    var i = 0
    while(i<t.length){
      var tmp = t.apply(i)
      println(tmp.getName +", Knotengewicht: "+ tmp.getWeight +", Pos: "+ tmp.getPos +", Neg: "+ tmp.getNeg)
      i = i+1
    }

    
    getEdgeWeight(module0)

    module0.forall({clause: FOLClause => clause.literals.exists(
      {literal : FOLNode => (literal match {
        case PositiveFOLLiteral(posL) => posL.top;
        case NegativeFOLLiteral(negL) => negL.top;
      }).substring(0, 2) == "O0"})})

    List(
      CNFClauseStore(module0)
      )
  }

  def getNodeWeight(clauses: CNFClauseStore) = {
    var pos = getPos(clauses)
    pos = getPredicates(pos)
    var neg = getNeg(clauses)
    neg = getPredicates(neg)
    val nodeWeight = getPredicateOccurence(pos, neg)
    nodeWeight
  }

  def getEdgeWeight(clauses: CNFClauseStore) = {
    var clauselist = getClauseList(clauses)
    println(getEdges(clauselist))
  }

  /**
   * get all positive literals from the clausestore in a list
   */
  def getPos(clauses: CNFClauseStore) = {
    var c = clauses
    var pos = List[String]()
    while(!c.isEmpty){
      var tmp = c.head.positiveLiterals.toArray
      var size = tmp.size
      var i = 0
      while(i < size){
        pos = pos ::: List(tmp(i).toString)
        i = i + 1
      }
      c = c.tail
    }
    pos
  }

  /**
   * get all negative literals from the clausestore in a list
   */
  def getNeg(clauses: CNFClauseStore) = {
    var c = clauses
    var neg = List[String]()
    while(!c.isEmpty){
      var tmp = c.head.negativeLiterals.toArray
      var size = tmp.size
      var i = 0
      while(i < size){
        neg = neg ::: List(tmp(i).toString)
        i = i + 1
      }
      c = c.tail
    }
    neg
  }

  /**
   * extracts predicates from a literal list
   */
  def getPredicates(literal: List[String]) = {
    var lit = literal
    var l = List[String]()
    val posregex = """(\w+)\(?.*""".r
    val negregex = """(¬\()(\w+)\(?.*""".r
    while(!lit.isEmpty){
      lit.head match {
        case posregex(a) => l = l ::: List(a)
        case negregex(a, b) => l = l ::: List(b)
        case _ => println("kein Match: "+ lit.head)
      }
      lit = lit.tail
    }
    l
  }

  /**
   * checks how often a predicate is in the list
   */
  def getPredicateOccurence(pos: List[String], neg: List[String]): List[Node] = {
    var nodes: List[Node] = List()
    var p = pos
    while(!p.isEmpty){
      var i: Int = findInList(nodes, p.head)
      if(i == -1){
        nodes = nodes ::: List(new Node(p.head, 1, 1, 0))
      }
      else{
        nodes.apply(i).setPos(nodes.apply(i).getPos + 1)
        nodes.apply(i).setWeight
      }
      p = p.tail
    }
    
    var n = neg
    while(!n.isEmpty){
      var i: Int = findInList(nodes, n.head)
      if(i == -1){
        nodes = nodes ::: List(new Node(n.head, 1, 0, 1))
      }
      else{
        nodes.apply(i).setNeg(nodes.apply(i).getNeg + 1)
        nodes.apply(i).setWeight
      }
      n = n.tail
    }
    return nodes
  }
  
  def findInList(nodes: List[Node], node: String): Int = {
    var i: Int = 0
    while(i<nodes.length){
      if(nodes.apply(i).getName.equals(node)){
        return i
      }
      i = i+1
    }
    return -1
  }

  def getClauseList(clauses: CNFClauseStore) = {
    var clauselist = List[List[String]]()
    var c = clauses
    while(!c.isEmpty){
      var tmp = c.head.absoluteLiterals.toArray
      var size = tmp.size
      var i = 0
      var clause = List[String]()
      while(i < size){
        clause = clause ::: List(tmp(i).toString)
        i = i + 1
      }
      clauselist = clauselist ::: List(clause)
      c = c.tail
    }
    clauselist
  }

  def getEdges(clauselist: List[List[String]]) = {
    var cl = clauselist
    var edges = List[List[String]]()
    while(!cl.isEmpty){
      var s = cl.head.size
      var t = 0
      var x = ""
      var y = ""
      val regex = """(\w+)\(?.*""".r
      while(t<s-1){
        var u = t+1
        cl.head(t) match {
            case regex(a) => x = a
            case _ => println("kein Match: "+ cl.head(t))
        }
        while(u<s){
          cl.head(u) match {
            case regex(a) => y = a
            case _ => println("kein Match: "+ cl.head(t))
          }
          edges = edges ::: List(List(x, y))
          u = u + 1
        }
        t = t + 1
      }
      cl = cl.tail
    }
    edges
  }


  def getEdgeOccurence(edges: List[List[String]]) = {
    
  }


}
