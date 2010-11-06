package partitioning

import core.containers.{CNFClauseStore, ClauseStorage, Node, Edge}
import domain.fol.ast.{NegativeFOLLiteral, PositiveFOLLiteral, FOLNode, FOLClause}
import domain.fol.parsers.SPASSIntermediateFormatParser
import helpers.Logging
import java.io._



class Partition extends ClauseStoragePartitioning with Logging{

  val parser = SPASSIntermediateFormatParser

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

    val g = newGraph(getEdgeWeight(module0), getNodeWeight(module0))
    printGraph(g, "/home/tk/hiwi/DIRE/input/conf/test.dfg")


    module0.forall({clause: FOLClause => clause.literals.exists(
      {literal : FOLNode => (literal match {
        case PositiveFOLLiteral(posL) => posL.top;
        case NegativeFOLLiteral(negL) => negL.top;
      }).substring(0, 2) == "O0"})})

    List(
      CNFClauseStore(module0)
      )
  }

  /**
   * returns all nodes from a clausestore with their weight
   */
  def getNodeWeight(clauses: CNFClauseStore) = {
    var pos = getPos(clauses)
    pos = getPredicates(pos)
    var neg = getNeg(clauses)
    neg = getPredicates(neg)
    var nodeWeight: List[Node] = getPredicateOccurence(pos, neg)
    nodeWeight = nodeWeight sort (_ > _)   // sortBy would be better
    nodeWeight
  }

  /**
   * returns all edges from a clausestore with their weight
   */
  def getEdgeWeight(clauses: CNFClauseStore) = {
    var edges: List[Edge] = getEdgeOccurence(getEdges(clauses))
    edges = edges sort (_ > _)   // sortBy would be better
    edges
  }

  /**
   * returns all positive literals from the clausestore in a list
   */
  def getPos(clauses: CNFClauseStore) = {
    var c = clauses
    var pos = List[String]() //store for the positive literals
    while(!c.isEmpty){ //while there are clauses left in the clausestore get positive literals and store them in pos
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
   * returns all negative literals from the clausestore in a list
   */
  def getNeg(clauses: CNFClauseStore) = {
    var c = clauses
    var neg = List[String]() //store for the negative literals
    while(!c.isEmpty){ //while there are clauses left in the clausestore get negative literals and store them in neg
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
    while(!p.isEmpty){  //checks how often the positive predicate is in pos
      var i: Int = findInList(nodes, p.head)
      if(i == -1){
        nodes = nodes ::: List(new Node(p.head, 1, 1, 0, false, List[Node]()))
      }
      else{
        nodes.apply(i).setPos(nodes.apply(i).getPos + 1)
        nodes.apply(i).setWeight
      }
      p = p.tail
    }

    var n = neg
    while(!n.isEmpty){ //checks how often the negative predicate is in neg
      var i: Int = findInList(nodes, n.head)
      if(i == -1){
        nodes = nodes ::: List(new Node(n.head, 1, 0, 1, false, List[Node]()))
      }
      else{
        nodes.apply(i).setNeg(nodes.apply(i).getNeg + 1)
        nodes.apply(i).setWeight
      }
      n = n.tail
    }
    return nodes
  }

  /**
   * finds position of the node in a list of nodes
   */
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

  /**
   * returns all edges of a clausestore in a list
   */
  def getEdges(clauses: CNFClauseStore) = {
    var c = clauses
    val posregex = """(\w+)\(?.*""".r
    var edges = List[List[String]]()
    var x = ""
    var y = ""
    while(!c.isEmpty){
      var tmp = c.head.absoluteLiterals.toArray
      var size = tmp.size
      var i = 0
      while(i < size - 1){
        var j = i + 1
        tmp(i).toString match {
            case posregex(a) => x = a
            case _ => println("kein Match: "+ tmp(i))
        }
        while(j < size){
          tmp(j).toString match {
            case posregex(a) => y = a
            case _ => println("kein Match: "+ tmp(j))
          }
          edges = edges ::: List(List(x, y))
          j = j + 1
        }
        i = i + 1
      }
      c = c.tail
    }
    edges
  }


  /**
   * returns how often an edge is used in the graph
   */
  def getEdgeOccurence(edges: List[List[String]]): List[Edge] = {
    var edgeocurrence: List[Edge] = List()
    var e = edges
    while(!e.isEmpty){
      var i: Int = findEdgeInList(edgeocurrence, e.head)
      if(i == -1){
        edgeocurrence = edgeocurrence ::: List(new Edge(e.head, 1))
      }
      else{
        edgeocurrence.apply(i).setOccurence(edgeocurrence.apply(i).getOccurence + 1)
      }
      e = e.tail
    }

    return edgeocurrence
  }

  /**
   * finds specific edge in a list of edges
   */
  def findEdgeInList(edges: List[Edge], edge: List[String]): Int = {
    var i: Int = 0
    while(i<edges.length){
      if(edges.apply(i).getNodes.equals(edge)){
        return i
      }
      i = i+1
    }
    return -1
  }

  /**
   * Creates new graph from a list of edges and their nodes
   */
  def newGraph(edges: List[Edge], nodes: List[Node]) = {
    var newGraph = List[Edge]()
    var e = edges
    var n = nodes
    val nodenum = n.length
    var one = List[String]()
    var two = List[String]()
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
          newGraph = newGraph ::: List(e.head)
          var i: Int = findInList(n, node1)
          n.apply(i).setCon(true)
          i = findInList(n, node2)
          n.apply(i).setCon(true)
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
          newGraph = newGraph ::: List(new Edge(List(tmp.getName, one.apply(0)), 0))
          x = x + 1
          two = two ::: List(one.apply(0))
          one = one.drop(1)
          one = one ::: List(tmp.getName)
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
          newGraph = newGraph ::: List(new Edge(List(one.apply(i), one.apply(j)), 0))
          x = x + 1
          i = -1
          j = one.length   //break
        }
        j = j + 1
      }
      i = i + 1
      j = i + 1
    }

    println("Number of nodes: "+ nodenum)
    println("Number of inserted edges: "+ x)
    println("Number of edges in the original graph: "+ edges.length)
    println("Nodes without edges:")
    i = 0
    while(i<n.length){
      var tmp = n.apply(i)
      if(!tmp.getCon){println(tmp.getName +", Nodeweight: "+ tmp.getWeight +", Pos: "+ tmp.getPos +", Neg: "+ tmp.getNeg)}
      i = i+1
    }
    println()
    println("Nodes with one edge: "+ one)
    println("Nodes with two edges: "+ two)
    println("New graph:")
    i = 0
    while(i<newGraph.length){
      var tmp = newGraph.apply(i)
      println(tmp.getNodes +" occurrence old graph: "+ tmp.getOccurence)
      i = i+1
    }
    println()
    newGraph
  }

  /**
   * Checks if their is already a connection between two nodes
   */
  def edgeCon(node1: String, node2: String,graph: List[Edge], one: List[String]): Boolean = {
    if(node1.equals(node2)){
      return true
    }
    var i = findNode(graph, node1)
    if(i == -1){
      return false
    }
    var g = graph.take(i) ::: graph.drop(i + 1)
    if(graph.apply(i).getNodes.apply(0).equals(node1) || graph.apply(i).getNodes.apply(0).equals("¬"+ node1)){
      var n = graph(i).getNodes.apply(1)
      return edgeCon(n, node2, g, one)
    }
    else{
      var n = graph(i).getNodes.apply(0)
      return edgeCon(n, node2, g, one)
    }

  }

  /**
   * find position of the node in a list of edges
   */
  def findNode(edges: List[Edge], node:String): Int = {
    var i = 0
    var e = edges
    while(!e.isEmpty){
      if(e.head.getNodes.contains(node)){
        return i
      }
      if(e.head.getNodes.contains("¬"+ node)){
        return i
      }
      i = i + 1
      e = e.tail
    }
    return -1
  }

  /**
   * prints the graph into the file
   */
  def printGraph(g: List[Edge], file: String) = {
    var i = 0
    val bufferedWriter = new BufferedWriter(new FileWriter(file))
    try {
      bufferedWriter.write("list_of_clauses().\n")
      while(i<g.length){
        var tmp = g.apply(i)
        var n = tmp.getNodes
        bufferedWriter.write("  clause( || ")
        bufferedWriter.write(n.apply(0) +" -> ")
        bufferedWriter.write(n.apply(1) +").\n")
        i = i+1
      }
      bufferedWriter.write("end_of_list. ")
    } finally {
      try {
      bufferedWriter.close()
      } catch { case _ => }
    }
  }

   /*

   //noch viel zu langsam
   //indexOf ersetzen, weil es wesentlich langsamer ist, wie eine Funktion, welche die Liste durchsucht

   def getLiterals(clauses: CNFClauseStore): List[Edge] = {
     var c = clauses
     var edges = List[Edge]()
     var clause = List[String]() //store for the literals of  a clause
     while(!c.isEmpty){ //while there are clauses left in the clausestore get positive literals and store them in pos
       var tmp = c.head.positiveLiterals.toArray  //get positive literals
       var size = tmp.size
       var i = 0
       while(i < size){
        clause= clause ::: List(tmp(i).toString)
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



   def getPredicateOccurence(literals: List[String]): List[Edge] = {
     val posregex = """(\w+)\(?.*""".r
     val negregex = """¬\((\w+)\(?.*""".r
     var l = literals
     var edges = List[Edge]()
     var d = List[Node]()
     while(!l.isEmpty){
        l.head match {
        case posregex(a) => d = d ::: List(getNode(a, true))
        case negregex(a) => d = d ::: List(getNode(a, false))
        case _ => println("kein Match: "+ l.head)
        }
        l = l.tail            //prüfen, ob ich schon früher mit getedges anfangen kann
        edges =  edges ::: getEdges(d)
    }
    edges
  }

  /**
   * updates the node with the name and returns the node
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



  def getEdges(clauses: List[Node]): List[Edge] = {
    var edge = List[Node]()
    var edges = List[Edge]()
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
  def getEdgeOccurence(edge: List[Node], edges: List[Edge]): List[Edge] = {
    var edgeocurrence = edges
    var i: Int = edges.indexOf(edge)
    if(i == -1){
        edgeocurrence = edgeocurrence ::: List(new Edge(edge, 1))
    }
    else{
        edgeocurrence.apply(i).setOccurence(edgeocurrence.apply(i).getOccurence + 1)
    }
    return edgeocurrence
  }


  /**
   * Creates new graph from a list of edges and their nodes
   */
  def newGraph(edges: List[Edge]) = {
    var newGraph = List[Edge]()
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
          newGraph = newGraph ::: List(new Edge(List(tmp, one.apply(0)), 0))
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
          newGraph = newGraph ::: List(new Edge(List(one.apply(i), one.apply(j)), 0))
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
  def edgeCon(node1: Node, node2: Node,graph: List[Edge], one: List[Node]): Boolean = {
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
