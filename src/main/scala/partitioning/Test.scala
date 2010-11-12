package partitioning

import core.containers.{CNFClauseStore, ClauseStorage, Node, EdgeTest}
import domain.fol.ast.{NegativeFOLLiteral, PositiveFOLLiteral, FOLNode, FOLClause}
import domain.fol.parsers.SPASSIntermediateFormatParser
import scala.collection.immutable.{HashMap}
import helpers.Logging
import java.io._



class Test extends ClauseStoragePartitioning with Logging{

  val parser = SPASSIntermediateFormatParser
  var nodes = List[Node]()   // die liste könnte man durch hashnodes.value ersetzen, hashnodes.value liefert einen iterator mit allen knoten
  var hashnodes = HashMap[String, Node]()
  var hashedges = HashMap[Node, EdgeTest]()
  
  override def partition(clauses: ClauseStorage) = {
    val module0 = SPASSIntermediateFormatParser.parseFromFile(new File("input/conf/aminoacid_clauses.dfg"))

    val g = test(module0)


    module0.forall({clause: FOLClause => clause.literals.exists(
      {literal : FOLNode => (literal match {
        case PositiveFOLLiteral(posL) => posL.top;
        case NegativeFOLLiteral(negL) => negL.top;
      }).substring(0, 2) == "O0"})})

    List(
      CNFClauseStore(module0)
      )
  }


  def test(clauses: CNFClauseStore) = {
    val x = getClauses(clauses)
    println("Kantenanzahl: "+x.size)
    println("Knotenanzahl: "+nodes.size)
    var tmp = nodes sort (_ > _)
    while(!tmp.isEmpty){
      println(tmp.head.getName+" "+ tmp.head.getWeight+" "+tmp.head.getPos+ " "+ tmp.head.getNeg)
      tmp = tmp.tail
    }
    x
  }

   def getClauses(clausestore: CNFClauseStore) = {
     var c = clausestore
     var clauses = List[List[Node]]()
     var edges = List[EdgeTest]()
     while(!c.isEmpty){
       var clause = List[String]()
       var tmp = c.head.positiveLiterals.toArray
       var size = tmp.size
       var i = 0
       while(i < size){
         clause = clause ::: List(tmp(i).toString)
         i = i + 1
       }
       tmp = c.head.negativeLiterals.toArray
       size = tmp.size
       i = 0
       while(i < size){
         clause = clause ::: List(tmp(i).toString)
         i = i + 1
       }
       var x = getPredicateOccurence(clause)
       clauses = clauses ::: List(x)
       edges = edges ::: getEdges(x)
       c = c.tail

     }
     //clauses
     edges
   }

   def getPredicateOccurence(literals: List[String]): List[Node] = {
     val posregex = """(\w+)\(?.*""".r
     val negregex = """¬\((\w+)\(?.*""".r
     var l = literals
     var clause = List[Node]()
     while(!l.isEmpty){
        l.head match {
        case posregex(a) => clause = clause ::: List(getNode(a, true))
        case negregex(a) => clause = clause ::: List(getNode(a, false))
        case _ => println("kein Match: "+ l.head)
        }
        l = l.tail  
    }
    clause
  }

  /**
   * updates the node with the name and returns the node
   */
  def getNode(name: String, pos: boolean): Node = {
    if(hashnodes.contains(name)){
      if(pos){
        var node = hashnodes(name)
        node.setPos(node.getPos+1)
        node.setWeight
        return node
      }
      else{
        var node = hashnodes(name)
        node.setNeg(node.getNeg+1)
        node.setWeight
        return node
      }
    }
    else{
       if(pos){
        val node = new Node(name, 1, 1, 0, false, List[Node]())
        nodes = nodes ::: List(node)
        hashnodes = hashnodes + (name -> node)
        return node
      }
      else{
        val node = new Node(name, 1, 0, 1, false, List[Node]())
        nodes = nodes ::: List(node)
        hashnodes = hashnodes + (name -> node)
        return node
       }
    }
  }

  //ab hier muss noch umgeschrieben werden

   //arbeitet noch nicht richtig, gibt viel zu viele kanten zurück
  def getEdges(clause: List[Node]): List[EdgeTest] = {
    var edge = List[Node]()
    var edges = List[EdgeTest]()
    var tmp = clause
    var size = tmp.size
    var i = 0
    while(i < size - 1){
      var j = i + 1
      var x = tmp.apply(i)
      while(j < size){
        var y = tmp.apply(j)
        x.addNeighbour(y)
        y.addNeighbour(x)
        edges = getEdgeOccurence(List(x, y), edges)
        j = j + 1
      }
      i = i + 1
    }
    edges
  }

  /**
   * returns how often an edge is used in the graph
   */
  def getEdgeOccurence(edge: List[Node], edges: List[EdgeTest]): List[EdgeTest] = {
    var edgeocurrence = edges
    val i = findEdge(edge, edges)
    if(i == -1){
        edgeocurrence = edgeocurrence ::: List(new EdgeTest(edge, 1))
    }
    else{
        edgeocurrence.apply(i).setOccurence(edgeocurrence.apply(i).getOccurence + 1)
    }
    return edgeocurrence
  }

  def findEdge(edge: List[Node], edges: List[EdgeTest]): Int = {
    var e = edges
    val node1 = edge.head
    val node2 = edge.tail.head
    var i = 0
    while(!e.isEmpty){
      var nodes = e.head.getNodes
      if (e.head.getNodes.contains(node1) && e.head.getNodes.contains(node2)){
        return i
      }
      e = e.tail
    }
    return -1
  }

}