package ca.mcmaster.lasci.front_end

sealed abstract class Param

case class NamedParam (ident: Ident) extends Param

case object VarArg extends Param