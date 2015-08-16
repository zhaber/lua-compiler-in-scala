package ca.mcmaster.lasci

sealed abstract class Operation  
sealed abstract class BinOperation extends Operation 
sealed abstract class UnaryOperation extends Operation 
sealed abstract case class ArithmeticOperation() extends BinOperation
sealed abstract case class CompareOperation() extends BinOperation
sealed abstract case class EqualityOperation() extends BinOperation
sealed abstract case class BooleanOperation() extends BinOperation 

case object Plus extends ArithmeticOperation {
  override def toString = "+"
}

case object Minus extends ArithmeticOperation {
  override def toString = "-"
}

case object Multipl extends ArithmeticOperation {
  override def toString() = "*"
}

case object Div extends ArithmeticOperation {
  override def toString() = "/"
}

case object Power extends ArithmeticOperation {
  override def toString() = "^"
}

case object Mod extends ArithmeticOperation {
  override def toString() = "%"
}

case object Concat extends BinOperation {
  override def toString() = ".."
}

case object Less extends CompareOperation {
  override def toString() = "<"
}

case object LessEq extends CompareOperation {
  override def toString() = "<="
}

case object More extends CompareOperation {
  override def toString() = ">"
}

case object MoreEq extends CompareOperation {
  override def toString() = ">="
}

case object Eq extends EqualityOperation {
  override def toString() = "=="
}

case object NotEq extends EqualityOperation {
  override def toString() = "~="
}

case object And extends BooleanOperation {
  override def toString() = "and"
}

case object Or extends BooleanOperation {
  override def toString() = "or"
}

case object Negation extends UnaryOperation {
  override def toString() = "-"
}

case object Not extends UnaryOperation {
  override def toString() = "not"
}

case object Length extends UnaryOperation {
  override def toString() = "#"
}