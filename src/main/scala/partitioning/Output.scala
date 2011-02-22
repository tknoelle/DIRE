package partitioning

import java.io._
import core.containers.{Node, Edge}
import scala.collection.immutable.{HashMap}

class Output {

  /**
   *
   *  prints the graph into the file
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

  /**
   * Prints the predicates with its number
   */
    def printPredicates(p: List[Node], file:String){
      var pr = p
      val bufferedWriter = new BufferedWriter(new FileWriter(file+".net"))
      try {
        bufferedWriter.write("*Vertices "+p.size +"\n")
        while(!pr.isEmpty){
          bufferedWriter.write(pr.head.getNum+" "+pr.head.getName+" "+pr.head.getWeight)
          bufferedWriter.newLine
          pr = pr.tail
        }
      } finally {
        try {
        bufferedWriter.close()
        } catch { case _ => }
      }
    }

  /**
   * Prints the constants, functions and predicates with its number
   */
    def printVertices(constants:List[Node], functions:List[Node], predicates: List[Node], file:String){
      var c = constants
      var f = functions
      var p = predicates
      var num = 1
      val size = c.size + f.size + p.size
      val bufferedWriter = new BufferedWriter(new FileWriter(file+".net"))
      try {
        bufferedWriter.write("*Vertices "+size +"\n")
        while(!c.isEmpty){
          bufferedWriter.write(num+" "+c.head.getName)
          bufferedWriter.newLine
          c = c.tail
          num = num + 1
        }
        while(!f.isEmpty){
          bufferedWriter.write(num+" "+f.head.getName)
          bufferedWriter.newLine
          f = f.tail
          num = num + 1
        }
        while(!p.isEmpty){
          bufferedWriter.write(num+" "+p.head.getName)
          bufferedWriter.newLine
          p = p.tail
          num = num + 1
        }
      } finally {
        try {
        bufferedWriter.close()
        } catch { case _ => }
      }
    }

  /**
   * Prints the Partition of the predicates
   */
  def printPartitions(nodes: List[Node], file:String){
      var pr = nodes
      val bufferedWriter = new BufferedWriter(new FileWriter(file+".clu"))
      try {
        bufferedWriter.write("*Vertices "+nodes.size)
        bufferedWriter.newLine
        while(!pr.isEmpty){
          bufferedWriter.write(pr.head.getPartition+"")
          bufferedWriter.newLine
          pr = pr.tail
        }
      } finally {
        try {
        bufferedWriter.close()
        } catch { case _ => }
      }
  }

  /**
   * Prints the Partition of the constants, functions and predicates
   */
  def printPartitionsCFP(constants:List[Node], functions:List[Node], predicates: List[Node], file:String){
      var c = constants
      var f = functions
      var p = predicates
      val size = c.size + f.size + p.size
      val bufferedWriter = new BufferedWriter(new FileWriter(file+".clu"))
      try {
        bufferedWriter.write("*Vertices "+size)
        bufferedWriter.newLine
        while(!c.isEmpty){
          bufferedWriter.write(c.head.getPartition+"")
          bufferedWriter.newLine
          c = c.tail
        }
        while(!f.isEmpty){
          bufferedWriter.write(f.head.getPartition+"")
          bufferedWriter.newLine
          f = f.tail
        }
        while(!p.isEmpty){
          bufferedWriter.write(p.head.getPartition+"")
          bufferedWriter.newLine
          p = p.tail
        }
      } finally {
        try {
        bufferedWriter.close()
        } catch { case _ => }
      }
  }

  /**
   * Prints the graph in an input format for metis
   */
  def printMetis(nodes: List[Node], hashedges:Map[String, Edge], file:String){
    var n = nodes
    val bufferedWriter = new BufferedWriter(new FileWriter(file+".graph"))
    try {
      bufferedWriter.write(nodes.size +" "+hashedges.size+" 11")
      bufferedWriter.newLine
      while(!n.isEmpty){
        bufferedWriter.write(" "+n.head.getWeight)
        var neighbours = n.head.getNeighbours
        while(!neighbours.isEmpty){
          bufferedWriter.write(" "+neighbours.head.getNum)
          val n1 = n.head.getName + neighbours.head.getName
          val n2 = neighbours.head.getName + n.head.getName
          if(hashedges.contains(n1)){
            bufferedWriter.write(" "+hashedges(n1).getOccurence)
          }
          else if(hashedges.contains(n2)){
            bufferedWriter.write(" "+hashedges(n2).getOccurence)
          }
          else{
              bufferedWriter.write(" 0")
          }

          neighbours = neighbours.tail
        }
        bufferedWriter.newLine
        n = n.tail
      }
    } finally {
      try {
      bufferedWriter.close()
      } catch { case _ => }
    }
  }

  /**
   * Prints all functions with frequency
   */
  def printPrecedence(pre: List[Node], file: String){
     var f = pre
      val bufferedWriter = new BufferedWriter(new FileWriter(file+".precedence"))
      try {
        bufferedWriter.write("set_precedence(")
        bufferedWriter.write(f.head.getName)
        f = f.tail
        while(!f.isEmpty){
          bufferedWriter.write(","+f.head.getName)
          //bufferedWriter.newLine
          f = f.tail
        }
        bufferedWriter.write(")")
      } finally {
        try {
        bufferedWriter.close()
        } catch { case _ => }
      }
  }


}