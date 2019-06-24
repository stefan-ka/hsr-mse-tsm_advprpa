package ch.mse

class Queue[+A] private (elements: List[A]) {
  def dequeue(): (A, Queue[A]) = (elements.head, new Queue[A](elements.tail))
  def enqueue[R >: A](element: R) : Queue[R] = new Queue(elements :+ element)
}

object Queue {
  def apply[A](elements: A*): Queue[A] = new Queue(elements.toList)
}
