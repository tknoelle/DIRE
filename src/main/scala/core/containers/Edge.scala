package core.containers


class Edge(val nodes: List[Node], /*val nodes: Tupel[Node], */var occurence: Int) extends Ordered[Edge] {
  def getNodes() = nodes
  def getOccurence() = occurence
  def setOccurence(newoc: Int) = occurence = newoc
  def compare(otherEdge: Edge) = {
      occurence.compare(otherEdge.occurence)
    }
}
