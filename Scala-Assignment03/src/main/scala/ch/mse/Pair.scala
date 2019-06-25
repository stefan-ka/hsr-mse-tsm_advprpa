package ch.mse

class Pair[+T,+S](val first: T, val second: S) {
  def replaceFirst[U >: T](newFirst: U): Pair[U,S] = new Pair(newFirst, second)
  def replaceSecond[U >: S](newSecond: U): Pair[T,U] = new Pair(first, newSecond)
}

object Pair {
  def swap[T,S](pair: Pair[T,S]) = new Pair(pair.second, pair.first)
}
