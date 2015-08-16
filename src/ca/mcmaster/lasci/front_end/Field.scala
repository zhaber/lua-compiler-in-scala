package ca.mcmaster.lasci.front_end

sealed abstract class Field
	
case class NamedField(ident: Ident) extends Field

case class ExpressionField(exp: Expression) extends Field

case object UnnamedField extends Field