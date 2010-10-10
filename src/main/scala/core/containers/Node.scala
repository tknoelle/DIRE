package core.containers


class Node(val name: String, var weight: Int, var pos: Int, var neg: Int, var con: Boolean) extends Ordered[Node]
{
  def getName() = name
  def getWeight() = weight
  def getPos() = pos
  def getNeg() = neg
  def getCon() = con
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

}
