package ca.mcmaster.lasci.front_end

import scala.collection.mutable.LinkedHashMap
import ca.mcmaster.lasci.front_end._
import scala.util.parsing.input.Positional

sealed abstract class Statement extends Positional

case object Break extends Statement

case class Do (statements: List[Statement]) extends Statement

case class For(i: Ident, exp1: Expression, exp2: Expression, exp3: Expression, block: List[Statement]) extends Statement

case class Foreach(i: List[Ident], exps: List[Expression], block: List[Statement]) extends Statement

case class If(ifExpBlocks: LinkedHashMap[Expression, List[Statement]], elseBlock: List[Statement]) extends Statement

case class Repeat (exp: Expression, block: List[Statement]) extends Statement

case class Return(expressions: List[Expression]) extends Statement

case class VarsAssignment(variables : List[Variable], values: List[Expression]) extends Statement

case class LocalVarDeclarations(idents : List[Ident], exps: List[Expression]) extends Statement

case class While(exp: Expression, block: List[Statement]) extends Statement

case class Function(functionName: FunctionName, body: FunctionBody) extends Statement {
	val name = functionName.idents.foldLeft("")(_ + _.name + ".").init
}

case class LocalFunction(ident: Ident, body: FunctionBody) extends Statement

case class FunctionCall(functionCallExp: FunctionCallExpression) extends Statement