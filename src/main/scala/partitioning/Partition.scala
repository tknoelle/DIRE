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

    //println(getPredicates(module0))
    var predicates = getPredicates(module0)
    var pos = List[String]()
    var neg = List[String]()
    val posregex = """(\w+)""".r
    val negregex = """(¬\()(\w+)""".r
    while(!predicates.isEmpty){
      predicates.head match {
        case posregex(a) => pos = pos ::: List(a)
        case negregex(a, b) => neg = neg ::: List(b)
        case _ => println("kein Match: "+ predicates.head)
      }
      predicates = predicates.tail
    }
    //println(pos)
    //println(neg)
    val t = getPredicateOccurence(pos, neg)
    var i = 0
    while(i<t.length){
      var tmp = t.apply(i)
      println(tmp.getName +", Knotengewicht: "+ tmp.getWeight +", Pos: "+ tmp.getPos +", Neg: "+ tmp.getNeg)
      i = i+1
    }


    /*
    module0.forall({clause: FOLClause => clause.literals.exists(
      {literal : FOLNode => (literal match {
        case PositiveFOLLiteral(posL) => posL.top;
        case NegativeFOLLiteral(negL) => negL.top;
      }).substring(0, 2) == "O0"})})
      */


    List(
      CNFClauseStore(module0)
      )
  }



  def getPredicates(clauses: CNFClauseStore) = {
    val regex = """\[?(¬?\(?\w+)\([^()]*\(?[^()]*\)?\)\)?\]?""".r
    var array = clauses.head.toString.split('V')
    val i = array.length
    val regex(node) = array(0)
    var predicates = List(node)
    var t = 1
    while(t<i){
      val regex(node) = array(t)
      predicates = predicates ::: List(node)
      t = t+1
    }
    var c = clauses.tail
    while(!c.isEmpty){
      array = c.head.toString.split('V')
      var t = 0
      while(t<i){
        array(t) match{
          case "[¬(" => array(t + 1) = array(t) + "V" +array(t+1); t = t+1
          case "¬(" => array(t + 1) = array(t) + "V" +array(t+1); t = t+1
          case _ =>
        }
        val regex(node) = array(t)
        predicates = predicates ::: List(node)
        t = t+1
      }
      c = c.tail
    }

           //[¬(Hydrophobicity(U))VHydrophobic(U)VHydrophilic(U)]
    //val reg3 =  """\[(¬?\(?\w+)\([^()]*\(?[^()]*\)?\)\)?V?(¬?\(?\w+)\([^()]*\(?[^()]*\)?\)\)?V?(¬?\(?\w+)\([^()]*\(?[^()]*\)?\)\)\]""".r
    //if(nodeCount(clauses.head.toString) == 1){
    //val reg2(node1, node2) = clauses.head.toString
    //var predicates = List(node1, node2)
    //}
    //else if(nodeCount(clauses.head.toString) == 2){
     // val reg3(node1, node2, node3) = clauses.head.toString
      //var predicates = List(node1, node2, node3)
    //}
    //var c = clauses.tail
    //while(!c.isEmpty){
    //  if(nodeCount(clauses.head.toString) == 1){
    //    val reg2(node1, node2) = c.head.toString
    //    predicates = predicates ::: List(node1, node2)
    //  }
    //  else if(nodeCount(clauses.head.toString) == 2){
    //    val reg3(node1, node2, node3) = clauses.head.toString
     //   predicates = List(node1, node2, node3)
     // }
     // c = c.tail
   // }
    predicates
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
  /*
  def nodeCount(subject: String)={
	var index = subject.length - 1
	var count = 0
	while(index >= 0){
		if(subject.charAt(index) == 'V') count += 1
		index -= 1
	}
	count
}
   */
  /*def updateList(i: Int, nodes: List[Node], posorneg: Int):List[Node] = {
     var x = 0
     var res = List[Node]()
     while(x < nodes.length){
       if(x == i && posorneg == 0){
         val t = nodes.apply(x).getPos + 1
         res = res ::: List(new Node(nodes.apply(x).getName, nodes.apply(x).setWeight(t, nodes.apply(x).getNeg), t, nodes.apply(x).getNeg))
       }
       else if(x == i && posorneg == 1){
         val t = nodes.apply(x).getNeg + 1
         res = res ::: List(new Node(nodes.apply(x).getName, nodes.apply(x).setWeight(nodes.apply(x).getPos, t), nodes.apply(x).getPos, t))
       }
       else{
         res = res ::: List(new Node(nodes.apply(x).getName, nodes.apply(x).getWeight, nodes.apply(x).getPos, nodes.apply(x).getNeg))
       }
       x = x + 1
     }
    res
  }*/


}
