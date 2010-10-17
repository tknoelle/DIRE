package core.containers

               // für nodes Node anstatt String benutzen und Tupel als Ersatz für die Liste
class Edge(val nodes:List[String], /*val nodes: List[Node],*/ /*val nodes: Tupel[Node], */var occurence: Int) extends Ordered[Edge] {
  def getNodes() = nodes
  def getOccurence() = occurence
  def setOccurence(newoc: Int) = occurence = newoc
  def compare(otherEdge: Edge) = {
      occurence.compare(otherEdge.occurence)
    }
}
