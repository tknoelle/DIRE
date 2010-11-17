package partitioning

import core.containers.{CNFClauseStore, ClauseStorage, Node, Edge}
import domain.fol.ast.{NegativeFOLLiteral, PositiveFOLLiteral, FOLNode, FOLClause}
import domain.fol.parsers.SPASSIntermediateFormatParser
import scala.collection.immutable.{HashMap}
import helpers.Logging
import java.io._



class Partition extends ClauseStoragePartitioning with Logging{

  val parser = SPASSIntermediateFormatParser
  var nodes = List[Node]()   // die liste könnte man durch hashnodes.value ersetzen, hashnodes.value liefert einen iterator mit allen knoten
  var hashnodes = HashMap[String, Node]()
  var edges = List[Edge]()
  var hashedges = HashMap[String, Edge]()

  override def partition(clauses: ClauseStorage) = {
    val module0 = SPASSIntermediateFormatParser.parseFromFile(new File("input/conf/aminoacid_clauses.dfg"))


    getGraph(module0)
    val g = newGraph()
    printGraph(g, "/home/tk/hiwi/DIRE/input/conf/output")
    printPredicates(nodes, "/home/tk/hiwi/DIRE/input/conf/output")

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
     * Gets all nodes and edges from the graph
     */
    def getGraph(clauses: CNFClauseStore) = {
      val x = getClauses(clauses)
      nodes = nodes sort (_ > _)
      edges = edges sort (_ > _)
    }

     def getClauses(clausestore: CNFClauseStore) = {
       var c = clausestore
       var clauses = List[List[Node]]()
       var edges = List[Edge]()
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
         //clauses = clauses ::: List(x)
         //edges = edges ::: getEdges(x)
         edgesTest(x)
         c = c.tail

       }
       //clauses
       //edges
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
          val node = new Node(name, 1, 1, 0, false, List[Node](), 0)
          nodes = nodes ::: List(node)
          hashnodes = hashnodes + (name -> node)
          return node
        }
        else{
          val node = new Node(name, 1, 0, 1, false, List[Node](), 0)
          nodes = nodes ::: List(node)
          hashnodes = hashnodes + (name -> node)
          return node
         }
      }
    }

    def edgesTest(clause: List[Node]) = {
      var edge = List[Node]()
      var edges = List[Edge]()
      var tmp = clause
      var size = tmp.size
      var i = 0
      while(i < size){
        var j = i + 1
        var x = tmp.apply(i)
        while(j < size){
          var y = tmp.apply(j)

          // falls hier schon Nachbarn nötig entkommentieren
          //x.addNeighbour(y)
          //y.addneighbour(x)
          edgeoccurence(x, y)
          j = j + 1
        }
        i = i + 1
      }
    }

    def edgeoccurence(x: Node, y: Node) = {
      val n1 = y.getName + x.getName
      val n2 = x.getName + y.getName
      var edge = new Edge(List[Node](x, y), 1)
      if(hashedges.contains(n1)){
        edge = hashedges(n1)
        edge.setOccurence(edge.getOccurence + 1)
      }
      else if(hashedges.contains(n2)){
        edge = hashedges(n2)
        edge.setOccurence(edge.getOccurence + 1)
      }
      else{
        edges = edges ::: List(edge)
        hashedges = hashedges + (n1 -> edge)
      }
    }



    /**
     * Creates new graph from a list of edges and their nodes
     */
    def newGraph() = {
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
            newGraph = newGraph ::: List(e.head)
            node1.setCon(true)
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
      println("Nodes with one edge: ")
      var z = one
      while(!z.isEmpty){
        println(z.head.getName)
        z = z.tail
      }
      println()
      println("Nodes with two edges: ")
      z = two
      while(!z.isEmpty){
        println(z.head.getName)
        z = z.tail
      }
      println()
      println("New graph:")
      i = 0
      while(i<newGraph.length){
        var tmp = newGraph.apply(i)
        println(tmp.getNodes.head.getName +" "+ tmp.getNodes.tail.head.getName +" occurrence old graph: "+ tmp.getOccurence)
        i = i+1
      }
      println()
      newGraph
    }

    /**
     * Checks if their is already a connection between two nodes
     */
    def edgeCon(node1: Node, node2: Node, graph: List[Edge], one: List[Node]): Boolean = {
      if(node1.equals(node2)){
        return true
      }
      var i = findNode(graph, node1)
      if(i == -1){
        return false
      }
      var g = graph.take(i) ::: graph.drop(i + 1)
      if(graph.apply(i).getNodes.apply(0).equals(node1)){
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
    def findNode(edges: List[Edge], node:Node): Int = {
      var i = 0
      var e = edges
      while(!e.isEmpty){
        if(e.head.getNodes.contains(node)){
          return i
        }
        i = i + 1
        e = e.tail
      }
      return -1
    }


  def setPartitions(number: Int) = {
    var n = nodes
    n
  }


  //should move to output class

    /**
     * prints the graph into the file
     */
    def printGraph(g: List[Edge], file: String) = {
      var i = 0
      val bufferedWriter = new BufferedWriter(new FileWriter(file+".dire"))
      try {
        bufferedWriter.write("list_of_clauses().\n")
        while(i<g.length){
          var tmp = g.apply(i)
          var n = tmp.getNodes
          bufferedWriter.write("  clause( || ")
          bufferedWriter.write(n.apply(0).getName +" -> ")
          bufferedWriter.write(n.apply(1).getName +").\n")
          i = i+1
        }
        bufferedWriter.write("end_of_list. ")
      } finally {
        try {
        bufferedWriter.close()
        } catch { case _ => }
      }
    }

    def printPredicates(p: List[Node], file:String){
      var i = 1
      var pr = p
      val bufferedWriter = new BufferedWriter(new FileWriter(file+".net"))
      try {
        bufferedWriter.write("*Vertices "+p.size +"\n")
        while(!pr.isEmpty){
          bufferedWriter.write(i+" "+pr.head.getName +"\n")
          i = i+1
          pr = pr.tail
        }
      } finally {
        try {
        bufferedWriter.close()
        } catch { case _ => }
      }
    }

}
