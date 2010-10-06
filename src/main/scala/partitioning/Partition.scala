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
    var pos = getPos(module0)
    pos = getPredicates(pos)
    var neg = getNeg(module0)
    neg = getPredicates(neg)
    val t = getPredicateOccurence(pos, neg)
    var i = 0
    while(i<t.length){
      var tmp = t.apply(i)
      println(tmp.getName +", Knotengewicht: "+ tmp.getWeight +", Pos: "+ tmp.getPos +", Neg: "+ tmp.getNeg)
      i = i+1
    }

    module0.forall({clause: FOLClause => clause.literals.exists(
      {literal : FOLNode => (literal match {
        case PositiveFOLLiteral(posL) => posL.top;
        case NegativeFOLLiteral(negL) => negL.top;
      }).substring(0, 2) == "O0"})})

    List(
      CNFClauseStore(module0)
      )
  }


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


}
