package ch.mse.app.types.path

object Arrays {
  
  case class OfLength(n: Int) {
     class Arr {
        private val a: Array[Int] = new Array[Int](n)
        def apply(x: Int) = a(x)
        def update(x: Int, y: Int): Unit = {a(x) = y}
     }
  }
  
  val ofLength4 = OfLength(4)
  val ofLength5 = OfLength(5)
  
  type ArrayOfLength4 = ofLength4.Arr
  type ArrayOfLength5 = ofLength5.Arr
  
  def f(a: ArrayOfLength5): Unit = {a(4) = 4 }
  
  val a4 = new ArrayOfLength4
  val a5 = new ArrayOfLength5
  
//  f(a4)
  f(a5)
  
}