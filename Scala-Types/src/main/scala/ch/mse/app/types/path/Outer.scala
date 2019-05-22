package ch.mse.app.types.path

class Outer {
  class Inner
  def put(inner: this.Inner) {}
}

object Outer {
  val o1 = new Outer
  val o2 = new Outer

  def foo(x: o1.Inner) = 3
//  def foo(x: o2.Inner) = 4 // problem due to erasure
  
  
  def main(args: Array[String]) {
    
    val i1 = new o1.Inner
    val i2 = new o2.Inner
    
    foo(i1);
    // foo(i2); // type mismatch
    
  }
}