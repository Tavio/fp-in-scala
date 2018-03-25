def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
  if(sup.size < sub.size) return false
  else {
    if (sup.zip(sub).filter((x) => x._1 == x._2).size == sub.size) return true
    else return hasSubsequence(sup.tail, sub)
  }
}