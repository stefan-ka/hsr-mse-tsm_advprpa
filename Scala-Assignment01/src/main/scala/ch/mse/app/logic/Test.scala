package ch.mse.app.logic

import Interpreter.eval

object Test {

  def main(args: Array[String]) {
    val intDiv = CpsCmd(List(
      AssiCmd("q", LitAExpr(0)),
      AssiCmd("r", IdAExpr("m")),
      WhileCmd(
        RelBExpr(GreaterEq, IdAExpr("r"), IdAExpr("n")),
        CpsCmd(List(
          AssiCmd("q", DyaAExpr(Plus, IdAExpr("q"), LitAExpr(1))),
          AssiCmd("r", DyaAExpr(Minus, IdAExpr("r"), IdAExpr("n")))
        ))
      ),
      SkipCmd
    ));

    val state = Map[String, Int]("m" -> 5, "n" -> 2);
    println("state = " + state);
    println("res = " + eval(intDiv, state));
  }

}