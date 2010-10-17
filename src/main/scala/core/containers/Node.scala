package core.containers

 //con unnötig
class Node(val name: String, var weight: Int, var pos: Int, var neg: Int, var con: Boolean, var neighbours: List[Node]) extends Ordered[Node]
{
  def getName() = name
  def getWeight() = weight
  def getPos() = pos
  def getNeg() = neg
  def getCon() = con
  def getNeighbours() = neighbours
  def getNumberOfNeighbours() = neighbours.length
  def setWeight() = {
    if(pos >= neg){
      weight = ((pos + neg)/((pos - neg) + 1))
    }
    else{
      weight = ((pos + neg)/((neg - pos) + 1))
    }
  }
  def setPos(newpos: Int) = pos = newpos
  def setNeg(newneg: Int) = neg = newneg
  def setCon(newcon: Boolean) = con = newcon
  def compare(otherNode: Node) = {
      weight.compare(otherNode.weight)
    }
  def setNeighbours(newneighbours: List[Node]) = neighbours = newneighbours
  def addNeighbour(newneighbour: Node) = neighbours = neighbours ::: List(newneighbour)

}
