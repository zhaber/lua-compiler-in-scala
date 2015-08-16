package ca.mcmaster.lasci.front_end

import scala.collection.mutable.LinkedHashMap
import ca.mcmaster.lasci.BinOperation
import ca.mcmaster.lasci.UnaryOperation

sealed abstract class Expression

case object NilExpression extends Expression

case object True extends Expression

case object False extends Expression

case class FloatNumber(number: Double) extends Expression

case class StringExpression(string: String) extends Expression

case class LambdaExpression(lambda: Lambda) extends Expression

case class TableExpression(fields: List[(Field, Expression)]) extends Expression

case class UnOpExpression(operation: UnaryOperation, exp: Expression) extends Expression

case object VarArgExpression extends Expression

case class BinOpExpression(exp1: Expression, operation: BinOperation, exp2: Expression) extends Expression

case class IdentExpression(ident: Ident) extends Expression

case class FunctionCallExpression(exps: List[Expression], argsExp: FunctionArgsExpression) extends Expression

case class VariableExpression(exps: List[Expression]) extends Expression

abstract sealed class PrefixExpressionSuffix extends Expression

abstract sealed class VariableSuffix extends PrefixExpressionSuffix

case class TableIndexExpression(index: Expression) extends VariableSuffix

case class DotIdentExpression(suffix: Ident) extends VariableSuffix

case class FunctionArgsExpression(args: List[(Field, Expression)]) extends PrefixExpressionSuffix

case class MethodArgsExpression(ident: Ident, methodArgs: List[(Field, Expression)]) extends FunctionArgsExpression(methodArgs)