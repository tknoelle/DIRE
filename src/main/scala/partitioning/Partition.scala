package partitioning

import core.containers.{CNFClauseStore, ClauseStorage, Node, Edge}
import domain.fol.parsers.SPASSIntermediateFormatParser
import scala.collection.immutable.HashMap
import helpers.Logging
import java.io._
import core.ordering.{ALCLPOComparator, LazyLexicographicPrecedence}
import domain.fol.ast.FOLNode
import scala.collection.mutable.{Map => MMap}

class Partition extends ClauseStoragePartitioning with Logging {

  val parser = SPASSIntermediateFormatParser
  var nodes = List[Node]()
  var hashnodes = HashMap[String, Node]()
  var edges = List[Edge]()
  var hashedges = HashMap[String, Edge]()
  var functions = List[Node]()
  var hashfunctions = HashMap[String, Node]()
  var constants = List[Node]()
  var hashconstants = HashMap[String, Node]()
  var visited = List[Node]()
  var unorderedClauses = 0
  var cache: MMap[(String, String), Int] = scala.collection.mutable.HashMap[(String, String), Int]()

  def parsertest() = {
    SPASSIntermediateFormatParser.parseDFGFromFile(new File("input/conf/test.dfg"))
  }

  override def partition(clauses: ClauseStorage) = {


    val module0 = SPASSIntermediateFormatParser.parseDFGClauseFromFile(new File("input/conf/aminoacid.dfg"))

    val prec = SPASSIntermediateFormatParser.parseDFGPrecedenceFromFile(new File("input/conf/aminoacid.dfg"))
    println(prec)

    var i = 0
    while (i < prec.size - 1) {
      cache = cache + ((prec(i), prec(i + 1)) -> 1)
      i = i + 1
    }


    val out = new Output

    val partitions = 12;

    getClauses(module0, 1)
    val g = newGraph()
    //addC(partitions)
    //hashnodes("partof").setCustomWeight(34715)
    setPartitions(partitions, nodes)
    setPartitions(partitions, constants)
    setPartitions(partitions, functions)
    propertyHierarchy


    //printGraph(g, "/home/tk/hiwi/DIRE/input/conf/output")
    out.printVertices(constants, functions, nodes, "output/test")
    val precedence = setPrecedence
    out.printPartitionsCFP(constants, functions, nodes, "output/test")
    out.printMetis(nodes, hashedges, "output/test")
    out.printPrecedence(precedence, "output/test")
    out.printPredicates(nodes, "output/test")
    //addC(partitions)
    //out.printMetis(nodes, hashedges, "/home/tk/hiwi/DIRE/input/conf/einfach_addC")

    /*module0.forall({clause: FOLClause => clause.literals.exists(
      {literal : FOLNode => (literal match {
        case PositiveFOLLiteral(posL) => posL.top;
        case NegativeFOLLiteral(negL) => negL.top;
      }).substring(0, 2) == "O0"})})       */

    List(
      CNFClauseStore(module0)
    )
  }


  /**
   * NW Partitioning called from the DIREShell
   */
  def nw(path: String, partitions: Int, output: String, property: Int, edges: Int) = {
    val module0 = SPASSIntermediateFormatParser.parseDFGClauseFromFile(new File(path))
    val out = new Output
    getClauses(module0, edges)
    if (property == 1) {
      propertyHierarchy
    }
    setPartitions(partitions, nodes)
    out.printPredicates(nodes, "output/" + output)
    out.printPartitions(nodes, "output/" + output)
    if (unorderedClauses != 0) {
      println(unorderedClauses + " not complete ordered")
      unorderedClauses = 0
    }
  }

  /**
   * Metis Partitioning called from the DIREShell
   */
  def metis(path: String, output: String, property: Int, edges: Int) = {
    val out = new Output
    if (nodes.isEmpty) {
      val module0 = SPASSIntermediateFormatParser.parseDFGClauseFromFile(new File(path))
      getClauses(module0, edges)
      if (property == 1) {
        propertyHierarchy
      }
      out.printPredicates(nodes, "output/" + output)
    }
    out.printMetis(nodes, hashedges, "output/" + output)
    if (unorderedClauses != 0) {
      println(unorderedClauses + " not complete ordered")
      unorderedClauses = 0
    }
  }

  /**
   * Metis with addC called from the DIREShell
   */
  def metisaddC(path: String, partitions: Int, output: String, property: Int, edges: Int) = {
    val out = new Output
    if (nodes.isEmpty) {
      val module0 = SPASSIntermediateFormatParser.parseDFGClauseFromFile(new File(path))
      getClauses(module0, edges)
      if (property == 1) {
        propertyHierarchy
      }
      out.printPredicates(nodes, "output/" + output)
    }
    addC(partitions)
    out.printMetis(nodes, hashedges, "output/" + output + "_addC")
    if (unorderedClauses != 0) {
      println(unorderedClauses + " not complete ordered")
      unorderedClauses = 0
    }
  }

  /**
   * Creating a precedence called from the DIREShell
   */
  def precedence(path: String, output: String, property: Int, edges: Int) = {
    val out = new Output
    if (nodes.isEmpty) {
      val module0 = SPASSIntermediateFormatParser.parseDFGClauseFromFile(new File(path))
      getClauses(module0, edges)
      if (property == 1) {
        propertyHierarchy
      }
    }
    val precedence = setPrecedence
    out.printPrecedence(precedence, "output/" + output)
  }


  /**
   * Extracts predicates, functions and constants from a CNFClausestore and calls for every clause the occurence methods
   */
  def getClauses(clausestore: CNFClauseStore, literalComparison: Integer) = {
    var c = clausestore
    var clauses = List[List[Node]]()
    var edges = List[Edge]()
    while (!c.isEmpty) {
      var clause = Map[String, FOLNode]()
      var clausefunctions = List[String]()
      var clauseconstants = List[String]()
      var superproperties = List[String]()
      var subproperties = List[String]()
      var tmp = c.head.absoluteLiterals.toArray
      var size = tmp.size
      var i = 0
      while (i < size) {
        //clause = clause ::: List(tmp(i).top)     //use this instead of positive and negative, if positive or negative doesn't matter
        var f = tmp(i).args
        while (!f.isEmpty) {
          if (f.head != f.head.args.head) {
            clausefunctions = clausefunctions ::: List(f.head.top)
          }
          else if (f.head.top != "V" && f.head.top != "U" && f.head.top != "U1" && f.head.top != "U2") {
            clauseconstants = clauseconstants ::: List(f.head.top)
          }
          f = f.tail
        }
        i = i + 1
      }
      tmp = c.head.positiveLiterals.toArray
      size = tmp.size
      i = 0
      while (i < size) {
        clause = clause + (tmp(i).top -> tmp(i))
        superproperties = superproperties ::: List(tmp(i).top)
        i = i + 1
      }

      tmp = c.head.negativeLiterals.toArray
      size = tmp.size
      i = 0
      while (i < size) {
        //clause = clause ::: List(tmp(i).top)
        clause = clause + (tmp(i).top -> tmp(i))
        subproperties = subproperties ::: List(tmp(i).top.replace("-", ""))
        i = i + 1
      }

      var x = List[Node]()
      if (literalComparison == 1) {
        x = getPredicateOccurence(clause)
        literalCompare(clause)
      }
      else {
        x = getPredicateOccurence(clause)
        clauseToEdges(x)
      }

      getFunctionOccurence(clausefunctions)
      getConstantOccurence(clauseconstants)
      getSubProperties(superproperties, subproperties)
      //clauses = clauses ::: List(x)
      //edges = edges ::: getEdges(x)

      c = c.tail
    }
    //clauses
    //edges
  }

  /**
   * Orders the literals in the clause with LPO (core.ordering.ALCLPOComparator)
   */
  def literalCompare(predicates: Map[String, FOLNode]) {
    //work in progress
    var order = List[Node]()
    var unordered = List[Node]()
    var p = predicates.keySet.toList
    if (p.size == 0) {
      return List[Node]()
    }
    else if (p.size == 1) {
      return List[Node](hashnodes(p.head))
    }
    else {
      var a = predicates.apply(p.head)
      var aNode = hashnodes(p.head.replace("-", ""))
      p = p.tail
      var b = predicates.apply(p.head)
      var bNode = hashnodes(p.head.replace("-", ""))
      p = p.tail
      var z = PrecedenceComparator.literalComparator.compare(a, b).get
      if (z == 1) {
        order = List[Node](bNode, aNode)
      }
      else if (z == -1) {
        order = List[Node](aNode, bNode)
      }
      else {
        unordered = List[Node](aNode, bNode)
      }

      while (!p.isEmpty) {
        val c = predicates.apply(p.head)
        val cNode = hashnodes(p.head.replace("-", ""))
        p = p.tail
        z = PrecedenceComparator.literalComparator.compare(c, b).get
        if (z == 1) {
          z = PrecedenceComparator.literalComparator.compare(c, a).get
          if (z == 1) {
            order = List[Node](aNode, cNode)
            if (!unordered.contains(bNode)) {
              unordered = List[Node]()
            }
          }
          else if (z == -1) {
            order = List[Node](cNode, aNode)
            if (!unordered.isEmpty) {
              unordered = List[Node]()
            }
          }
          else {
            unordered = List[Node](cNode, aNode)
          }
        }
        else if (z == -1) {

        }
        else {
          unordered = List[Node](bNode, cNode)
        }
      }
      if (unordered.isEmpty) {
        var a = order.head
        order = order.tail
        var b = order.head
        order = order.tail
        edgeoccurence(a, b)
      }
      else {
        unorderedClauses = unorderedClauses + 1
        edgeoccurence(order.head, unordered.head)
        unordered = unordered.tail
        edgeoccurence(order.head, unordered.head)
      }
    }
  }

  def getFunctionOccurence(clausefunctions: List[String]) = {
    var f = clausefunctions
    while (!f.isEmpty) {
      if (hashfunctions.contains(f.head)) {
        var node = hashfunctions(f.head)
        node.setPos(node.getPos + 1)
        node.setWeight
      }
      else {
        val function = new Node(f.head, functions.size + 1, 1, 1, 0, false, List[Node](), 0, List[Node]())
        functions = functions ::: List(function)
        hashfunctions = hashfunctions + (f.head -> function)
      }
      f = f.tail
    }

  }

  def getSubProperties(superprop: List[String], subprop: List[String]) = {
    var sup = superprop
    while (!sup.isEmpty) {
      var sub = subprop
      while (!sub.isEmpty) {
        hashnodes(sup.head).addSubproperty(hashnodes(sub.head))
        sub = sub.tail
      }
      sup = sup.tail
    }

  }

  def getConstantOccurence(clauseconstants: List[String]) = {
    var c = clauseconstants
    while (!c.isEmpty) {
      if (hashconstants.contains(c.head)) {
        var node = hashconstants(c.head)
        node.setPos(node.getPos + 1)
        node.setWeight
      }
      else {
        val constant = new Node(c.head, constants.size + 1, 1, 1, 0, false, List[Node](), 0, List[Node]())
        constants = constants ::: List(constant)
        hashconstants = hashconstants + (c.head -> constant)
      }
      c = c.tail
    }

  }

  /**
   * returns a list of nodes
   */
  def getPredicateOccurence(literals: Map[String, FOLNode]): List[Node] = {
    val posregex = """(\w+)""".r
    val negregex = """-(\w+)""".r
    var l = literals.keySet.toList
    var clause = List[Node]()
    while (!l.isEmpty) {
      l.head match {
        case posregex(a) => clause = clause ::: List(getNode(a, true))
        case negregex(a) => clause = clause ::: List(getNode(a, false))
        case _ => println("kein Match: " + l.head)
      }
      l = l.tail
    }
    clause
  }

  /**
   * updates the node with the name and returns the node
   */
  def getNode(name: String, pos: boolean): Node = {
    if (hashnodes.contains(name)) {
      if (pos) {
        var node = hashnodes(name)
        node.setPos(node.getPos + 1)
        node.setWeight
        return node
      }
      else {
        var node = hashnodes(name)
        node.setNeg(node.getNeg + 1)
        node.setWeight
        return node
      }
    }
    else {
      if (pos) {
        val node = new Node(name, nodes.size + 1, 1, 1, 0, false, List[Node](), 0, List[Node]())
        nodes = nodes ::: List(node)
        hashnodes = hashnodes + (name -> node)
        return node
      }
      else {
        val node = new Node(name, nodes.size + 1, 1, 0, 1, false, List[Node](), 0, List[Node]())
        nodes = nodes ::: List(node)
        hashnodes = hashnodes + (name -> node)
        return node
      }
    }
  }

  /**
   * creates or updates the edges between the nodes
   */
  def clauseToEdges(clause: List[Node]) = {
    var edge = List[Node]()
    var edges = List[Edge]()
    var tmp = clause
    var size = tmp.size
    var i = 0
    while (i < size) {
      var j = i + 1
      var x = tmp.apply(i)
      while (j < size) {
        var y = tmp.apply(j)
        edgeoccurence(x, y)
        j = j + 1
      }
      i = i + 1
    }
  }

  /**
   * @param two nodes
   * creates or updates the edge between the two nodes
   */
  def edgeoccurence(x: Node, y: Node) = {
    val n1 = y.getName + x.getName
    val n2 = x.getName + y.getName
    var edge = new Edge(List[Node](x, y), 1)
    if (hashedges.contains(n1)) {
      edge = hashedges(n1)
      edge.setOccurence(edge.getOccurence + 1)
    }
    else if (hashedges.contains(n2)) {
      edge = hashedges(n2)
      edge.setOccurence(edge.getOccurence + 1)
    }
    else {
      x.addNeighbour(y)
      y.addNeighbour(x)
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
    var x = 0 //number of edges in the new graph
    while (!e.isEmpty) {
      var tmp = e.head.getNodes
      var node1 = tmp.apply(0)
      var node2 = tmp.apply(1)
      if (!two.contains(node1) && !two.contains(node2)) {

        if (!edgeCon(node1, node2, newGraph, one)) {
          if (one.contains(node1)) {
            two = two ::: List(node1)
            one = one -- List(node1) //should be filterNot
          }
          else {
            one = one ::: List(node1)
          }

          if (one.contains(node2)) {
            two = two ::: List(node2)
            one = one -- List(node2) //should be filterNot
          }
          else {
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
    // fills graph with random edges, if x < nodenum - 1

    var i = 0
    //nodes that have no edge get connected with a node with one edge
    while (i < n.length) {
      var tmp = n.apply(i)
      if (!tmp.getCon) {
        if (!one.isEmpty) {
          newGraph = newGraph ::: List(new Edge(List(tmp, one.apply(0)), 0))
          x = x + 1
          two = two ::: List(one.apply(0))
          one = one.drop(1)
          one = one ::: List(tmp)
        }
        tmp.setCon(true)
      }
      i = i + 1
    }
    i = 0
    var j = 1

    //till only two nodes left, nodes with only one edge get connected
    while (one.length > 2) {
      while (j < one.length) {
        if (!edgeCon(one.apply(i), one.apply(j), newGraph, one)) {
          two = two ::: List(one.apply(i))
          one = one -- List(one.apply(i))
          two = two ::: List(one.apply(j))
          one = one -- List(one.apply(j))
          newGraph = newGraph ::: List(new Edge(List(one.apply(i), one.apply(j)), 0))
          x = x + 1
          i = -1
          j = one.length //break
        }
        j = j + 1
      }
      i = i + 1
      j = i + 1
    }
    /*
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
    println()*/
    newGraph
  }

  /**
   * Checks if their is already a connection between two nodes
   */
  def edgeCon(node1: Node, node2: Node, graph: List[Edge], one: List[Node]): Boolean = {
    if (node1.equals(node2)) {
      return true
    }
    var i = findNode(graph, node1)
    if (i == -1) {
      return false
    }
    var g = graph.take(i) ::: graph.drop(i + 1)
    if (graph.apply(i).getNodes.apply(0).equals(node1)) {
      var n = graph(i).getNodes.apply(1)
      return edgeCon(n, node2, g, one)
    }
    else {
      var n = graph(i).getNodes.apply(0)
      return edgeCon(n, node2, g, one)
    }

  }

  /**
   * find position of the node in a list of edges
   */
  def findNode(edges: List[Edge], node: Node): Int = {
    var i = 0
    var e = edges
    while (!e.isEmpty) {
      if (e.head.getNodes.contains(node)) {
        return i
      }
      i = i + 1
      e = e.tail
    }
    return -1
  }

  /**
   * partitions the nodes in number of different partitions
   * the nodes are always put in the partition with the least weight
   */

  def setPartitions(number: Int, node: List[Node]) = {
    var n = node sort (_ > _)
    var x = new Array[Int](number)
    while (!n.isEmpty) {
      var i = 1
      var partition = 0
      while (i < number) {
        if (x(partition) > x(i)) {
          partition = i
        }
        i = i + 1
      }
      x(partition) = x(partition) + n.head.getWeight
      n.head.setPartition(partition)
      //println(n.head.getName +" "+ n.head.getWeight + " "+ n.head.getPartition)
      n = n.tail

    }
  }

  /**
   *  addC adds a constant (10000) to the n (number of partition) heaviest nodes
   */
  def addC(number: Int) = {
    var n = nodes sort (_ > _)
    var i = number
    while (i > 0) {
      n.head.addC
      n = n.tail
      i = i - 1
    }
  }

  /**
   *  creates a precedence as a list of nodes
   */
  def setPrecedence() = {
    var p = nodes sort (_ < _)
    var c = constants sort (_ < _)
    var f = functions sort (_ < _)
    var precedence = List[Node]()
    precedence = precedence ::: c
    precedence = precedence ::: f
    precedence = precedence ::: p
    precedence
  }

  /*def propertyHierarchy(){
    var n = nodes
    while(!n.isEmpty){
      var sub = n.head.getSubproperties
      while(!sub.isEmpty){
        val tmp = sub.head.getWeight
        sub.head.setWeight
        n.head.addWeight(sub.head.getWeight)
        sub.head.setCustomWeight(tmp)
        sub = sub.tail
      }
      n = n.tail
    }
  } */

  /**
   * adds the weight of all subproperties to a node
   */
  def propertyNode(node: Node): Node = {
    if (!visited.contains(node)) {
      visited = visited ::: List[Node](node)
      var tmp = node.getSubproperties
      while (!tmp.isEmpty) {
        propertyNode(tmp.head)
        node.addWeight(tmp.head.getWeight)
        tmp = tmp.tail
      }
    }
    node
  }

  /**
   * adds the weight of all subproperties to all nodes
   */
  def propertyHierarchy() = {
    var n = nodes
    while (!n.isEmpty) {
      if (visited.contains(n.head)) {
        n = n.tail
      }
      else {
        propertyNode(n.head)
        n = n.tail
      }
    }
  }


  object PrecedenceComparator {

    lazy val precedence = LazyLexicographicPrecedence
    precedence.setCache(cache)
    lazy val literalComparator = new ALCLPOComparator(this)
  }

}