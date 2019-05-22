package ch.mse.app.logic

import scala.collection.Map

object Interpreter {

  type Value = Int
  type State = Map[String, Value]

  def eval(command: Command, state: State): State =
    command match {
      case AssiCmd(id, expr) => state.updated(id, eval(expr, state))
      case CpsCmd(commands) => commands.foldLeft(state)((state, command) => eval(command, state))
      case WhileCmd(condition, command) => if (eval(condition, state)) eval(WhileCmd(condition, command), eval(command, state)) else state
      case SkipCmd => state
    }

  def eval(expr: ArithExpr, state: State): Int =
    expr match {
      case LitAExpr(value) => value
      case IdAExpr(id) => state.get(id).get
      case DyaAExpr(Times, expr1, expr2) => eval(expr1, state) * eval(expr2, state)
      case DyaAExpr(Div, expr1, expr2) => eval(expr1, state) / eval(expr2, state)
      case DyaAExpr(Mod, expr1, expr2) => eval(expr1, state) % eval(expr2, state)
      case DyaAExpr(Plus, expr1, expr2) => eval(expr1, state) + eval(expr2, state)
      case DyaAExpr(Minus, expr1, expr2) => eval(expr1, state) - eval(expr2, state)
    }

  def eval(expr: BoolExpr, state: State): Boolean =
    expr match {
      case LitBExpr(value) => value
      case NegBExpr(op) => !eval(op, state)
      case DyaBExpr(CondAnd, op1, op2) => eval(op1, state) && eval(op2, state)
      case DyaBExpr(CondOr, op1, op2) => eval(op1, state) || eval(op2, state)
      case RelBExpr(LessEq, expr1, expr2) => eval(expr1, state) <= eval(expr2, state)
      case RelBExpr(GreaterEq, expr1, expr2) => eval(expr1, state) >= eval(expr2, state)
    }

}