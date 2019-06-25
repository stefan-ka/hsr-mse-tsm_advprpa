package ch.mse

class PairM[T,S](private[this] var f: T, private[this] var s: S) {
  def first = f
  def second = s
  def replaceFirst(newFirst: T) { f = newFirst }
  def replaceSecond(newSecond: S) { s = newSecond }
}

object PairM {
  def swap[S](pair: PairM[S,S]) {
    val first = pair.first
    val second = pair.second
    pair.replaceFirst(second)
    pair.replaceSecond(first)
  }
}
