package core.containers

 //con unnötig
class Node(val name: String, var num: Int, var weight: Int, var pos: Int, var neg: Int, var con: Boolean, var neighbours: List[Node], var partition: Int) extends Ordered[Node]
{
  def getName() = name
  def getNum() = num
  def getWeight() = weight
  def getPos() = pos
  def getNeg() = neg
  def getCon() = con
  def getNeighbours() = neighbours
  def getNumberOfNeighbours() = neighbours.length
  def getPartition() = partition


  def setNum(newnum: Int) = num = newnum
  def setWeight() = weight = pos + neg
  def setCustomWeight(w: Int) = weight = w
  def addC() = weight = weight + 10000
  def einfach() = weight = pos + neg
  def setPos(newpos: Int) = pos = newpos
  def setNeg(newneg: Int) = neg = newneg
  def setCon(newcon: Boolean) = con = newcon
  def compare(otherNode: Node) = {
      weight.compare(otherNode.weight)
    }
  def setNeighbours(newneighbours: List[Node]) = neighbours = newneighbours
  def addNeighbour(newneighbour: Node) = neighbours = neighbours ::: List(newneighbour)
  def setPartition(newp: Int) = partition = newp
  
}
