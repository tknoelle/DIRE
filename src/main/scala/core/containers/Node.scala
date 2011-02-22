package core.containers

 //con unnötig
class Node(val name: String, var num: Int, var weight: Int, var pos: Int, var neg: Int, var con: Boolean, var neighbours: List[Node], var partition: Int, var subproperties: List[Node]) extends Ordered[Node]
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
  def getSubproperties() = subproperties


  def setNum(newnum: Int) = num = newnum
  def setWeight() = weight = pos + neg //Attention if this is changed, precedence will be wrong
  def addWeight(w: Int) = weight = weight + w
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
  def addSubproperty(sub: Node) = {
    if(!subproperties.contains(sub)){
      subproperties = subproperties ::: List(sub)
    }
  }
  def setSubproperties(sub: List[Node]) = subproperties = sub
  
}
