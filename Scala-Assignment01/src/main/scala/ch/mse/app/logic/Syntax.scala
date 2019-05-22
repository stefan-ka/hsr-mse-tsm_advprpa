package ch.mse.app.logic

abstract class BoolOperator 
case object CondAnd extends BoolOperator 
case object CondOr extends BoolOperator 

abstract class RelOperator
case object LessEq extends RelOperator
case object GreaterEq extends RelOperator

abstract class BoolExpr
case class LitBExpr(value: Boolean) extends BoolExpr
case class NegBExpr(expr: BoolExpr) extends BoolExpr
case class DyaBExpr(operator: BoolOperator, op1: BoolExpr, op2: BoolExpr) extends BoolExpr
case class RelBExpr(rel: RelOperator, expr1: ArithExpr, expr2: ArithExpr) extends BoolExpr

abstract class ArithOperator
case object Times extends ArithOperator
case object Div extends ArithOperator
case object Mod extends ArithOperator
case object Plus extends ArithOperator
case object Minus extends ArithOperator

abstract class ArithExpr
case class LitAExpr(value: Int) extends ArithExpr
case class IdAExpr(id: String) extends ArithExpr
case class DyaAExpr(operator: ArithOperator, expr1: ArithExpr, expr2: ArithExpr) extends ArithExpr

abstract class Command
case class AssiCmd(id: String, expr: ArithExpr) extends Command
case class CpsCmd(commands: List[Command]) extends Command
case class WhileCmd(condition: BoolExpr, command: Command) extends Command
case object SkipCmd extends Command
