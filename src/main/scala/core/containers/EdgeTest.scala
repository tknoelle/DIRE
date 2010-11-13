package core.containers

//praktischer wie eine liste von knoten, wäre ein tupel
class EdgeTest(val nodes: List[Node], var occurence: Int) extends Ordered[EdgeTest] {
  def getNodes() = nodes
  def getOccurence() = occurence
  def setOccurence(newoc: Int) = occurence = newoc
  def compare(otherEdge: EdgeTest) = {
      occurence.compare(otherEdge.occurence)
    }
}
