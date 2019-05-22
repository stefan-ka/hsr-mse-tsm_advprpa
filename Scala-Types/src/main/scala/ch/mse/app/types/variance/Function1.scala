package ch.mse.app.types.variance

trait Function1[T, R] {
  //Apply the body of this function to the argument.
  def apply(arg: T): R

  // Composes two instances of Function1 in a new Function1, 
  // with this function applied last.
  def compose[A](g: A => T): A => R = { x => apply(g(x)) }

  // Composes two instances of Function1 in a new Function1, 
  // with this function applied first.
  def andThen[A](g: R => A): T => A = { x => g(apply(x)) }

  override def toString() = "<function1>"
}


object TestFunction1 {
    def main(args: Array[String]) {
      val inc: Function1[Int, Int] = new Function1[Int, Int] {
        def apply(x: Int): Int = x+1
      }
      val sqr: Function1[Int, Int] =new Function1[Int, Int] {
        def apply(x: Int): Int = x*x
      }
      
      val f1 = inc.compose((x:Int) => x*x)
      println(f1(3)) // 10
      
      val f2 = inc.andThen((x: Int) => x*x)
      println(f2(3)) // 16
    }
}