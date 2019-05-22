package ch.mse.app.types.variance

trait Buildable[T] {
  def build: T
}

trait HeadBuilder extends Buildable[String] {
  var eyeCol = "brown"
  var hairCol = "red"

  def withEyeColor(c: String): this.type = { eyeCol = c;this}
  def withHairColor(c: String): this.type = { hairCol = c;this}

  def build = "eyes: " + eyeCol + ", hair: " + hairCol
}

trait BodyBuilder extends Buildable[String] {
  var legCnt = 4

  def withNumLegs(c: Int): this.type = { legCnt = c; this }

  def build = "legs: " + legCnt
}

class PersonBuilder extends HeadBuilder with BodyBuilder {
  override def build: String = List(super[BodyBuilder].build, super[HeadBuilder].build).mkString(", ")
}

object BuilderTest {
  def main(args: Array[String]): Unit = {
    val person = new PersonBuilder().withEyeColor("blue").withNumLegs(3).build
    println(person) // legs: 3, eyes: blue, hair: red
  }
}
