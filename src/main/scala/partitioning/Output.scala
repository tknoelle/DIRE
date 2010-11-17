package partitioning

import java.io._
import core.containers.{Node, Edge}

class Output {

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