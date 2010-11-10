package partitioning

import core.containers.{CNFClauseStore, ClauseStorage, Node, EdgeTest}
import domain.fol.ast.{NegativeFOLLiteral, PositiveFOLLiteral, FOLNode, FOLClause}
import domain.fol.parsers.SPASSIntermediateFormatParser
import scala.collection.immutable.{HashSet}
import helpers.Logging
import java.io._



class Test extends ClauseStoragePartitioning with Logging{

  val parser = SPASSIntermediateFormatParser
  var nodes = List[Node]()
  
  override def partition(clauses: ClauseStorage) = {
    val module0 = SPASSIntermediateFormatParser.parseFromFile(new File("input/conf/aminoacid_clauses.dfg"))



    /*val t = getNodeWeight(module0)
    var i = 0
    while(i<t.length){
      var tmp = t.apply(i)
      println(tmp.getName +", Knotengewicht: "+ tmp.getWeight +", Pos: "+ tmp.getPos +", Neg: "+ tmp.getNeg)
      i = i+1
    }

    val x = getEdgeWeight(module0)
    i = 0
    while(i<x.length){
      var tmp = x.apply(i)
      println(tmp.getNodes +", Häufigkeit: "+ tmp.getOccurence)
      i = i+1
    }      */

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
    val x = getLiterals(clauses)
    println("Kantenanzahl: "+x.size)
    println("Knotenanzahl: "+nodes.size)
    x
  }


   def getLiterals(clauses: CNFClauseStore) = {
     var c = clauses
     var edges = List[EdgeTest]()
     var clause = List[String]() //store for the literals of  a clause
     while(!c.isEmpty){ //while there are clauses left in the clausestore get positive literals and store them in pos
       var tmp = c.head.positiveLiterals.toArray  //get positive literals
       var size = tmp.size
       var i = 0
       while(i < size){
        clause = clause ::: List(tmp(i).toString)
        i = i + 1
       }
       tmp = c.head.negativeLiterals.toArray //get negative literals
       size = tmp.size
       i = 0
       while(i < size){
        clause = clause ::: List(tmp(i).toString)
        i = i + 1
       }
       edges = edges ::: getPredicateOccurence(clause)
       c = c.tail
     }
     edges
  }



   def getPredicateOccurence(literals: List[String]): List[EdgeTest] = {
     val posregex = """(\w+)\(?.*""".r
     val negregex = """¬\((\w+)\(?.*""".r
     var l = literals
     var edges = List[EdgeTest]()
     var d = List[Node]()
     while(!l.isEmpty){
        l.head match {
        case posregex(a) => //println(a)//d = d ::: List(getNode(a, true))
        case negregex(a) => //println(a)//d = d ::: List(getNode(a, false))
        case _ => println("kein Match: "+ l.head)
        }
        l = l.tail            //prüfen, ob ich schon früher mit getedges anfangen kann
        //edges =  edges ::: getEdges(d)
       edges = edges ::: List(new EdgeTest(d, 1))
    }
    edges
  }

  /**
   * updates the node with the name and returns the node
   *
   * methode noch viel zu langsam muss umgeschrieben werden
   */
  def getNode(name: String, pos: boolean): Node = {
    var i: Int = nodes.indexOf(name)
    if(pos){
     if(i == -1){
       val node = new Node(name, 1, 1, 0, false, List[Node]())
       nodes = nodes ::: List(node)
       return node
     }
     else{
       nodes.apply(i).setPos(nodes.apply(i).getPos + 1)
       nodes.apply(i).setWeight
       val node = nodes.apply(i)
       return node
     }
    }
    else{
      if(i == -1){
        val node = new Node(name, 1, 0, 1, false, List[Node]())
        nodes = nodes ::: List(node)
        return node
      }
      else{
        nodes.apply(i).setNeg(nodes.apply(i).getNeg + 1)
        nodes.apply(i).setWeight
        val node = nodes.apply(i)
        return node
      }
    }
  }

  def findNode(name: String): Int = {
    var n = nodes
    var i = 0
    while (!n.isEmpty){
      if(n.head.getName == name){
        return i
      }
      i = i+1
      n = n.tail
    }
    return -1
  }


   //arbeitet noch nicht richtig, gibt viel zu viele kanten zurück
  def getEdges(clauses: List[Node]): List[EdgeTest] = {
    var edge = List[Node]()
    var edges = List[EdgeTest]()
    var tmp = clauses
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
/*

  /**
   * Creates new graph from a list of edges and their nodes
   */
  def newGraph(edges: List[EdgeTest]) = {
    var newGraph = List[EdgeTest]()
    var e = edges
    var n = nodes
    val nodenum = n.length
    var one = List[Node]()
    var two = List[Node]()
    var x = 0   //number of edges in the new graph
    while(!e.isEmpty){
      var tmp = e.head.getNodes
      var node1 = tmp.apply(0)
      var node2 = tmp.apply(1)
      if(!two.contains(node1) && !two.contains(node2)){

        if(!edgeCon(node1, node2, newGraph, one)){
          if(one.contains(node1)){
            two = two ::: List(node1)
            one = one -- List(node1)        //should be filterNot
          }
          else{
            one = one ::: List(node1)
          }

          if(one.contains(node2)){
            two = two ::: List(node2)
            one = one -- List(node2)  //should be filterNot
          }
          else{
            one = one ::: List(node2)
          }
          node1.addNeighbour(node2)
          node2.addNeighbour(node1)
          newGraph = newGraph ::: List(e.head)
          node1.setCon(true)     //unnötig
          node2.setCon(true)
          x = x + 1
        }
      }
      e = e.tail
    }
    // Graphen mit zufälligen Kanten füllen, wenn x < nodenum - 1

    var i = 0
    //nodes that have no edge get connected with a node with one edge
    while(i<n.length){
      var tmp = n.apply(i)
      if(!tmp.getCon){
        if(!one.isEmpty){
          newGraph = newGraph ::: List(new EdgeTest(List(tmp, one.apply(0)), 0))
          x = x + 1
          two = two ::: List(one.apply(0))
          one = one.drop(1)
          one = one ::: List(tmp)
        }
        tmp.setCon(true)
      }
      i = i+1
    }
    i = 0
    var j = 1

    //till only two nodes left, nodes with only one edge get connected
    while(one.length > 2){
      while(j < one.length){
        if(!edgeCon(one.apply(i), one.apply(j), newGraph, one)){
          two = two ::: List(one.apply(i))
          one = one -- List(one.apply(i))
          two = two ::: List(one.apply(j))
          one = one -- List(one.apply(j))
          newGraph = newGraph ::: List(new EdgeTest(List(one.apply(i), one.apply(j)), 0))
          x = x + 1
          i = -1
          j = one.length   //break
        }
        j = j + 1
      }
      i = i + 1
      j = i + 1
    }

    println("Anzahl an Knoten: "+ nodenum)
    println("Anzahl an eingefügten Kanten: "+ x)
    println("Anzahl an Kanten im Originalgraph: "+ edges.length)
    println("Knoten ohne Kanten:")
    i = 0
    while(i<n.length){
      var tmp = n.apply(i)
      if(!tmp.getCon){println(tmp.getName +", Knotengewicht: "+ tmp.getWeight +", Pos: "+ tmp.getPos +", Neg: "+ tmp.getNeg)}
      i = i+1
    }
    println()
    println("Knoten mit einer Kante: "+ one)
    println("Knoten mit zwei Kante: "+ two)
    println("Neuer Graph:")
    i = 0
    while(i<newGraph.length){
      var tmp = newGraph.apply(i)
      println(tmp.getNodes +" Häufigkeit alter Graph: "+ tmp.getOccurence)
      i = i+1
    }
    println()


  }

  /**
   * Checks if their is already a connection between two nodes
   */
  def edgeCon(node1: Node, node2: Node,graph: List[EdgeTest], one: List[Node]): Boolean = {
    if(node1 == node2){
      return true
    }
    var n = node1
    var m = node1
    while(n.getNumberOfNeighbours == 2){
      val x = n.getNeighbours
      if(x.apply(0) == node2 || x.apply(1) == node2 ){
        return true
      }
      else if(x.apply(0) == m){
        m = n
        n = x.apply(1)
      }
      else if(x.apply(1) == m){
        m = n
        n = x.apply(0)
      }
    }
    return false
  }

 */

}