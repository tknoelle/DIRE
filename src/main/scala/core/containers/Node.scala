package core.containers


class Node(val name: String, var weight: Int, var pos: Int, var neg: Int){
  def getName() = name
  def getWeight() = weight
  def getPos() = pos
  def getNeg() = neg
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
}
